CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-01T00:35:09Z creation;2017-10-01T00:35:13Z conversion to V3.1;2019-12-19T08:00:23Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171001003509  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_164                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�*6$8!�1   @�*6�>� @:��{J#:�d�-w1��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB  B  B  B   B(  B0  B8  B@  BHffBPffBX  B_��Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @xQ�@���@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B��\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{��C}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+��D,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDC~�DC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV��DWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�8�D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�8�D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�x�D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\D��)D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�  A�  A�  A�  A���A���A��A���A��A��A��mA���A�Aܴ9A�|�A���A�M�AڬA�I�A���A�%A���A�=qA�1'A�M�A�+A��A��#AÕ�A���A�ȴA�VA��A�~�A��DA�p�A��A�5?A��DA��TA�bA���A��DA��A��;A��7A���A�G�A�E�A��A��yA��`A��TA��\A�%A��hA�XA�?}A�{A�JA��FA�9XA�~�A�~�A�r�A��A�;dA���A�r�A���A��`A�;dA��#A���A��#A��7A�+A��-A���A�z�A���A�XA���A�Q�A��A��/A��7A��mA�?}A�JA��/A��hA�;dA���A�^5A� �A�t�A��AXA~��A~n�A}��A}�A|�DA{��AzAx�/AwAwoAv$�At�yAtffAs��As;dAr=qAqXAp�/Ap��Apv�Ap{Aop�Ao�An��AnJAm"�AlffAlbAkx�Aj�Ai|�AgƨAe�hAdVAc�Ab��Aa�Aa�A`�uA_�-A_A^ZA]��A];dA]�A\ �A[�AZ�RAZ^5AY�
AX��AX(�AW�PAV�DAU�mAUO�AS�mARȴARz�ARJAQ��AQoAPĜAP�+AP1'AO7LAMhsAMALr�AL  AK��AJĜAI�TAHQ�AG�-AFAE|�AE�PAD(�AC�mABĜAA&�A@��A@JA>�yA?t�A>�A=�TA=��A=�A<�A;A:��A:E�A:�A:bA9�TA8 �A6�/A5ƨA5
=A4I�A333A1��A1S�A1"�A/ƨA.A,1'A*ȴA*(�A(��A((�A'��A'�7A&��A&ffA&{A%��A%��A%|�A$�yA#��A!��A!|�AS�A�AȴA/A�;A��A�A�9A{A+A��A�RAhsA��AjA\)AbNA5?A�AbA
=A$�AK�A
z�A	�FA|�A�Ax�A�A�jA��A��AXAr�AJA��A ��A   @�l�@�x�@�K�@�{@��@��j@���@��`@�o@�J@���@�M�@���@��-@�?}@�@�1'@��@��
@���@�+@�X@���@�7L@�bN@�\)@�=q@�`B@�b@�l�@���@��@�V@�(�@�K�@�~�@���@���@�1@�S�@�j@���@Ѻ^@�G�@���@�bN@϶F@�M�@���@˕�@ʏ\@�E�@�hs@ȴ9@�1@�;d@��@���@�t�@°!@��T@��`@�  @�;d@�^5@���@���@��@�n�@���@���@�v�@�{@���@�r�@���@�o@��#@��/@�r�@���@��!@�z�@�dZ@��@� �@�K�@��\@���@�X@�%@�bN@�\)@��+@�-@�@���@��@��j@��
@�ff@�G�@�Ĝ@�Z@��@�l�@��@���@�^5@��@��@��D@�  @�t�@��y@��!@��\@�n�@�V@�E�@�=q@�J@��^@�`B@��@��D@�9X@�+@��!@���@�n�@�-@��T@���@�7L@��j@�1@��@��P@�dZ@�K�@�33@�ȴ@�E�@���@��7@�x�@�hs@��/@�r�@�A�@�1@��w@���@���@�|�@�K�@�+@�@��\@��@���@�%@��@���@��D@�r�@�bN@�Q�@�A�@�(�@�1@��;@��@��\@�-@���@���@�x�@�?}@��@��`@���@�z�@�A�@��
@��@�S�@�
=@�ȴ@��!@��+@�{@���@��@�b@�@��@��@~5?@}`B@|��@|�@|9X@{ƨ@{��@{��@{t�@{33@z��@y�7@x��@w�;@v��@vE�@v$�@u�h@u/@u�@t�j@s�m@s��@s��@s��@st�@r^5@q�@q�7@p��@pbN@pb@o�w@o��@o|�@o\)@oK�@n�R@m�@m��@m@m@m��@mV@l��@lj@l(�@k��@kƨ@k��@kt�@k@jM�@j�@i��@ix�@i7L@hr�@h �@g�;@g|�@g;d@g
=@fȴ@f�R@f��@f$�@e?}@d�j@dz�@d1@cdZ@c@b��@b��@a��@aX@a7L@`��@`Q�@`A�@`A�@`1'@_�@_�P@_l�@_�@^ȴ@^v�@]�T@]�-@]��@]��@]�h@]O�@\��@\�@[��@[dZ@["�@[o@Z�@Z��@Z^5@Z^5@Z�@Y�@Y��@Y�^@Y�^@Y��@YX@Y&�@X��@X��@W�;@V�R@Vv�@Vv�@VV@VE�@V{@U��@T�j@T�@S�
@S��@S�@SdZ@S33@R��@R�@Q��@Q7L@PĜ@PQ�@O�P@N�@Nff@N5?@N@M�T@M?}@L�@LZ@K�m@KdZ@K33@K"�@K@J�@JM�@Ihs@H��@HA�@G�w@G|�@G;d@G�@F��@F�y@F��@FE�@FE�@F$�@E@E`B@D�@D9X@C��@Cƨ@C��@C��@CdZ@CS�@CC�@B��@A�@A��@A��@A�^@A��@Ax�@Ax�@A7L@@��@@�`@@��@@bN@@A�@@A�@@ �@@b@@b@@  @?��@?�@?�@?��@?�@?�w@?��@?l�@?\)@?+@>��@>�@>ȴ@>��@>ff@>V@>@=p�@=`B@<��@<�@<Z@;��@;�m@;�
@;t�@:�@:��@:n�@:^5@:M�@:-@:�@:J@:J@9�@9�#@9G�@8r�@81'@8 �@7�@7\)@6�y@6�R@6E�@5��@5p�@4�@4�j@4�D@49X@3��@3S�@3"�@3@2�H@2n�@1x�@0�u@0bN@01'@0 �@0  @/�;@/�w@/��@/|�@/;d@/;d@/�@/
=@.��@.�@.�R@.��@.�+@.ff@.{@-�h@-p�@-p�@-`B@-?}@-V@,�/@,�j@,�@,�@,�@,��@,�D@,j@,9X@+��@+ƨ@+��@+�@+S�@+@*�H@*~�@*^5@*M�@*J@)��@)�#@)��@)�7@)x�@)&�@(Ĝ@(r�@'�w@'�P@'l�@';d@&�y@&��@&ff@&{@%��@%`B@%O�@$��@$�j@$��@$j@$Z@$(�@$1@#�
@#S�@"�H@"�H@"��@"��@"�!@"��@"�@!��@!��@!x�@!X@!7L@!%@ ��@ Q�@ A�@ 1'@ 1'@  �@ b@�;@�@l�@;d@��@v�@v�@$�@��@p�@?}@�@�@�/@�j@z�@ƨ@��@dZ@dZ@dZ@dZ@dZ@dZ@dZ@dZ@S�@33@"�@o@�@��@�\@M�@J@J@J@�7@X@�@��@r�@  @�w@+@�@
=@�y@�@ȴ@��@ff@{@@p�@V@�j@��@z�@�@��@o@�\@��@x�@x�@x�@x�@x�@x�@hs@%@�@r�@A�@1'@b@  @��@��@�P@�P@|�@l�@\)@;d@
=@�@ȴ@ȴ@�R@��@��@v�@E�@$�@@O�@�@�j@9X@��@�F@��@S�@C�@C�@33@@
��@
n�@
=q@
-@	��@	��@	��@	�^@	��@	��@	hs@	G�@	%@��@�@bN@Q�@A�@1'@ �@b@�;@�w@�P@+@�y@ȴ@��@v�@E�@$�@�@��@p�@`B@?}@V@�@�/@�@j@Z@I�@(�@(�@�@�m@�
@��@�@dZ@S�@33@o@�@�H@�H@�H@�H@��@��@�!@~�@M�@M�@=q@-@�@J@�#@��@hs@hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�  A�  A�  A�  A���A���A��A���A��A��A��mA���A�Aܴ9A�|�A���A�M�AڬA�I�A���A�%A���A�=qA�1'A�M�A�+A��A��#AÕ�A���A�ȴA�VA��A�~�A��DA�p�A��A�5?A��DA��TA�bA���A��DA��A��;A��7A���A�G�A�E�A��A��yA��`A��TA��\A�%A��hA�XA�?}A�{A�JA��FA�9XA�~�A�~�A�r�A��A�;dA���A�r�A���A��`A�;dA��#A���A��#A��7A�+A��-A���A�z�A���A�XA���A�Q�A��A��/A��7A��mA�?}A�JA��/A��hA�;dA���A�^5A� �A�t�A��AXA~��A~n�A}��A}�A|�DA{��AzAx�/AwAwoAv$�At�yAtffAs��As;dAr=qAqXAp�/Ap��Apv�Ap{Aop�Ao�An��AnJAm"�AlffAlbAkx�Aj�Ai|�AgƨAe�hAdVAc�Ab��Aa�Aa�A`�uA_�-A_A^ZA]��A];dA]�A\ �A[�AZ�RAZ^5AY�
AX��AX(�AW�PAV�DAU�mAUO�AS�mARȴARz�ARJAQ��AQoAPĜAP�+AP1'AO7LAMhsAMALr�AL  AK��AJĜAI�TAHQ�AG�-AFAE|�AE�PAD(�AC�mABĜAA&�A@��A@JA>�yA?t�A>�A=�TA=��A=�A<�A;A:��A:E�A:�A:bA9�TA8 �A6�/A5ƨA5
=A4I�A333A1��A1S�A1"�A/ƨA.A,1'A*ȴA*(�A(��A((�A'��A'�7A&��A&ffA&{A%��A%��A%|�A$�yA#��A!��A!|�AS�A�AȴA/A�;A��A�A�9A{A+A��A�RAhsA��AjA\)AbNA5?A�AbA
=A$�AK�A
z�A	�FA|�A�Ax�A�A�jA��A��AXAr�AJA��A ��A   @�l�@�x�@�K�@�{@��@��j@���@��`@�o@�J@���@�M�@���@��-@�?}@�@�1'@��@��
@���@�+@�X@���@�7L@�bN@�\)@�=q@�`B@�b@�l�@���@��@�V@�(�@�K�@�~�@���@���@�1@�S�@�j@���@Ѻ^@�G�@���@�bN@϶F@�M�@���@˕�@ʏ\@�E�@�hs@ȴ9@�1@�;d@��@���@�t�@°!@��T@��`@�  @�;d@�^5@���@���@��@�n�@���@���@�v�@�{@���@�r�@���@�o@��#@��/@�r�@���@��!@�z�@�dZ@��@� �@�K�@��\@���@�X@�%@�bN@�\)@��+@�-@�@���@��@��j@��
@�ff@�G�@�Ĝ@�Z@��@�l�@��@���@�^5@��@��@��D@�  @�t�@��y@��!@��\@�n�@�V@�E�@�=q@�J@��^@�`B@��@��D@�9X@�+@��!@���@�n�@�-@��T@���@�7L@��j@�1@��@��P@�dZ@�K�@�33@�ȴ@�E�@���@��7@�x�@�hs@��/@�r�@�A�@�1@��w@���@���@�|�@�K�@�+@�@��\@��@���@�%@��@���@��D@�r�@�bN@�Q�@�A�@�(�@�1@��;@��@��\@�-@���@���@�x�@�?}@��@��`@���@�z�@�A�@��
@��@�S�@�
=@�ȴ@��!@��+@�{@���@��@�b@�@��@��@~5?@}`B@|��@|�@|9X@{ƨ@{��@{��@{t�@{33@z��@y�7@x��@w�;@v��@vE�@v$�@u�h@u/@u�@t�j@s�m@s��@s��@s��@st�@r^5@q�@q�7@p��@pbN@pb@o�w@o��@o|�@o\)@oK�@n�R@m�@m��@m@m@m��@mV@l��@lj@l(�@k��@kƨ@k��@kt�@k@jM�@j�@i��@ix�@i7L@hr�@h �@g�;@g|�@g;d@g
=@fȴ@f�R@f��@f$�@e?}@d�j@dz�@d1@cdZ@c@b��@b��@a��@aX@a7L@`��@`Q�@`A�@`A�@`1'@_�@_�P@_l�@_�@^ȴ@^v�@]�T@]�-@]��@]��@]�h@]O�@\��@\�@[��@[dZ@["�@[o@Z�@Z��@Z^5@Z^5@Z�@Y�@Y��@Y�^@Y�^@Y��@YX@Y&�@X��@X��@W�;@V�R@Vv�@Vv�@VV@VE�@V{@U��@T�j@T�@S�
@S��@S�@SdZ@S33@R��@R�@Q��@Q7L@PĜ@PQ�@O�P@N�@Nff@N5?@N@M�T@M?}@L�@LZ@K�m@KdZ@K33@K"�@K@J�@JM�@Ihs@H��@HA�@G�w@G|�@G;d@G�@F��@F�y@F��@FE�@FE�@F$�@E@E`B@D�@D9X@C��@Cƨ@C��@C��@CdZ@CS�@CC�@B��@A�@A��@A��@A�^@A��@Ax�@Ax�@A7L@@��@@�`@@��@@bN@@A�@@A�@@ �@@b@@b@@  @?��@?�@?�@?��@?�@?�w@?��@?l�@?\)@?+@>��@>�@>ȴ@>��@>ff@>V@>@=p�@=`B@<��@<�@<Z@;��@;�m@;�
@;t�@:�@:��@:n�@:^5@:M�@:-@:�@:J@:J@9�@9�#@9G�@8r�@81'@8 �@7�@7\)@6�y@6�R@6E�@5��@5p�@4�@4�j@4�D@49X@3��@3S�@3"�@3@2�H@2n�@1x�@0�u@0bN@01'@0 �@0  @/�;@/�w@/��@/|�@/;d@/;d@/�@/
=@.��@.�@.�R@.��@.�+@.ff@.{@-�h@-p�@-p�@-`B@-?}@-V@,�/@,�j@,�@,�@,�@,��@,�D@,j@,9X@+��@+ƨ@+��@+�@+S�@+@*�H@*~�@*^5@*M�@*J@)��@)�#@)��@)�7@)x�@)&�@(Ĝ@(r�@'�w@'�P@'l�@';d@&�y@&��@&ff@&{@%��@%`B@%O�@$��@$�j@$��@$j@$Z@$(�@$1@#�
@#S�@"�H@"�H@"��@"��@"�!@"��@"�@!��@!��@!x�@!X@!7L@!%@ ��@ Q�@ A�@ 1'@ 1'@  �@ b@�;@�@l�@;d@��@v�@v�@$�@��@p�@?}@�@�@�/@�j@z�@ƨ@��@dZ@dZ@dZ@dZ@dZ@dZ@dZ@dZ@S�@33@"�@o@�@��@�\@M�@J@J@J@�7@X@�@��@r�@  @�w@+@�@
=@�y@�@ȴ@��@ff@{@@p�@V@�j@��@z�@�@��@o@�\@��@x�@x�@x�@x�@x�@x�@hs@%@�@r�@A�@1'@b@  @��@��@�P@�P@|�@l�@\)@;d@
=@�@ȴ@ȴ@�R@��@��@v�@E�@$�@@O�@�@�j@9X@��@�F@��@S�@C�@C�@33@@
��@
n�@
=q@
-@	��@	��@	��@	�^@	��@	��@	hs@	G�@	%@��@�@bN@Q�@A�@1'@ �@b@�;@�w@�P@+@�y@ȴ@��@v�@E�@$�@�@��@p�@`B@?}@V@�@�/@�@j@Z@I�@(�@(�@�@�m@�
@��@�@dZ@S�@33@o@�@�H@�H@�H@�H@��@��@�!@~�@M�@M�@=q@-@�@J@�#@��@hs@hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BF�BF�BF�BG�BH�BH�BL�BK�BM�BN�BN�BVBVBYBaHBiyBiyBl�Bl�BjBk�BhsBbNB_;BC�B��B�B��B�B�
BɺB�LB�3B�!B�-B��B��B��B��B��B��B�VB�B|�Bq�BiyBbNBN�BJ�B;dB0!B�BJB�B�BhB1B�B�B��BɺB�XB�hBt�Bk�BbNBZBP�BI�B<jB.B)�B�B�BoB�B\B\B
=B
��B
�B
�yB
�;B
��B
��B
ɺB
B
�^B
�dB
�XB
�?B
�-B
�B
��B
��B
��B
��B
��B
�{B
�\B
�DB
�+B
�B
{�B
q�B
k�B
gmB
bNB
]/B
VB
R�B
O�B
I�B
C�B
>wB
=qB
=qB
;dB
8RB
49B
0!B
-B
(�B
#�B
 �B
�B
�B
�B
JB
B	�B	�B	�sB	�fB	�HB	�#B	�B	��B	��B	��B	��B	ƨB	ƨB	��B	�RB	�LB	�9B	�'B	��B	��B	��B	��B	��B	�uB	�PB	�1B	�+B	�B	�B	}�B	|�B	z�B	u�B	o�B	cTB	e`B	bNB	_;B	[#B	S�B	M�B	?}B	8RB	.B	%�B	#�B	 �B	�B	�B	+B	1B		7B��B	+B	bB	1B	
=B	VB	
=B	B	  B	B	B	B	  B��B�B�B�B�yB�NB�/B�5B�)B��BɺBB�wB��B�^B�XB�^B�^B�?B�FB�?B�?B�-B�B��B��B�VB�PB�B� B�B}�B}�B�B|�Bw�Bs�Bk�BbNBgmBdZBhsBiyBaHB`BBdZB]/BT�BR�BP�BN�BO�BK�B<jBC�BK�BN�BK�BL�BG�BD�B@�BA�B?}B9XB<jB=qB8RB2-B49B49B5?B0!B$�B&�B+B$�B,B6FB6FB5?B49B6FB7LB7LB49B.B$�B �B&�B,B+B)�B)�B'�B-B.B+B+B,B-B-B.B,B)�B)�B�B(�B.B33B33B2-B0!B+B,B0!B1'B6FB49B5?B6FB5?B.B8RB>wB<jB=qB;dB=qB>wB@�B;dBE�BF�BH�BF�BB�BL�BT�BT�BQ�BVBXBVBYB^5B[#B[#BW
B\)B]/BgmBn�Bq�Bs�Bx�Bz�Bx�Bx�B}�B�B�+B�+B�%B�B�B�B�VB��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�9B�?B�?B�?B�9B�9B�?B�FB�LB�^B�^B��BȴBǮBȴBɺB��B��B��B��B�B�B�B�#B�#B�B�)B�;B�TB�fB�`B�TB�mB�B�B�B�B�B�B�B�B�B�B�B��B��B��B	B	B	B	B	B	B	B	B	B	B	  B	
=B	VB	\B	bB	oB	uB	{B	�B	{B	�B	�B	�B	�B	�B	�B	 �B	 �B	�B	!�B	#�B	&�B	1'B	1'B	0!B	-B	0!B	33B	5?B	5?B	7LB	9XB	9XB	9XB	9XB	8RB	9XB	>wB	?}B	B�B	D�B	I�B	H�B	J�B	M�B	M�B	M�B	R�B	S�B	S�B	R�B	P�B	VB	XB	ZB	_;B	aHB	aHB	bNB	bNB	bNB	bNB	bNB	e`B	k�B	l�B	l�B	k�B	k�B	o�B	q�B	r�B	t�B	u�B	u�B	u�B	t�B	u�B	z�B	z�B	|�B	}�B	}�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�%B	�%B	�7B	�DB	�=B	�DB	�\B	�bB	�hB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�3B	�9B	�?B	�?B	�FB	�RB	�wB	��B	�}B	��B	�}B	��B	��B	ĜB	ƨB	ǮB	ȴB	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�5B	�BB	�HB	�NB	�TB	�TB	�NB	�HB	�HB	�TB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
%B
%B
B
%B
1B
	7B

=B

=B

=B

=B

=B

=B

=B
	7B
	7B
	7B
JB
PB
PB
VB
VB
bB
bB
bB
hB
oB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
 �B
 �B
 �B
 �B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
,B
,B
,B
-B
-B
,B
,B
,B
,B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
33B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
6FB
6FB
6FB
8RB
8RB
8RB
8RB
8RB
7LB
7LB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
=qB
=qB
?}B
?}B
?}B
?}B
?}B
?}B
>wB
>wB
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
E�B
G�B
H�B
H�B
H�B
G�B
H�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
K�B
K�B
L�B
L�B
M�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
S�B
S�B
T�B
T�B
T�B
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
XB
XB
YB
XB
YB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BF�BF�BF�BG�BH�BH�BL�BK�BM�BN�BO(BVBVSBY�Bb�Bj�Bj�Bm�Bm�Bl"BlqBjBe�Be`BS@B�B�MBٚB�B��BοB�dB��B��B�tB��B�0B��B�B��B�eB��B��B�Bt�BlBe�BSuBN"B?�B2|B!-B(B�B�B�B�B��B�7B�:B��B�VB��Bw2Bm)Bc�B[=BR BK�B>�B1vB+�B"NB�B2B�BHB�B)B;B
�/B
�B
��B
��B
�BB
��B
�B
��B
�B
��B
�+B
�B
�B
��B
��B
�;B
��B
�?B
��B
�B
�JB
��B
��B
}VB
s�B
l�B
h�B
cnB
^jB
WYB
S�B
P�B
J�B
D�B
?}B
=�B
=�B
;�B
8�B
4�B
0�B
-�B
)�B
$�B
!�B
pB
�B
�B
B
SB	�?B	�B	��B	�B	�hB	�CB	��B	�,B	��B	͟B	˒B	�zB	�+B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	~�B	}qB	{dB	v�B	qB	ezB	fB	c B	`B	[�B	U2B	OBB	AoB	9rB	/�B	&�B	$ZB	"NB	yB	�B		B		B	
#B	 4B	B	hB		RB	
�B	�B	)B	SB	 B	�B	[B	aB	 �B��B�cB��B��B�B��B��B�B��B�B�0B��B�4B��B��B�xB�B��B�+B��B��B��B��B��B��B�kB��B��B��B�AB��B�B�B��B}�Bx�Bt�Bm)BdZBh�BfBi_BjKBb�Ba|Bd�B^jBW
BTaBRTBPHBQ4BMjB?cBEmBL~BO\BLdBMPBH�BE�BA�BBAB@iB:�B=�B>(B9�B3�B5B4�B5�B1AB&�B($B,B&fB-B6zB6�B5�B4�B6�B7�B7�B4�B/ B&�B"hB'�B,�B+�B*�B*�B(�B-�B.�B+�B+�B,�B-�B-�B.�B,�B*�B+B�B*B.�B3�B3�B2�B0�B,=B-)B1B1�B6�B4�B5�B6�B6+B/�B9$B>�B="B>B<PB>BB?.BAoB<�BF?BGEBIRBG�BC�BM�BU�BU�BR�BV�BX�BV�BY�B^�B[�B\)BX�B]B^�BhsBo5BraBtnBy>B{JBy�By�B~�B��B�zB��B��B��B��B�YB�B�
B�	B�CB�B�BB�'B�'B�;B��B�`B��B�B�qB�vB��B��B�tB�tB�tB��B��B��B��B��B��B�0B��B��B��B�B�#B�0B�<B�\B�bB�9B�7B�QB�=B�qBچBܒBߊB�B�B�B��B��B�B�B��B��B��B��B��B��B��B�B�GB�2B�RB�"B	;B	AB	AB	'B	AB	-B	-B	[B	[B	oB	 �B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 �B	!B	 BB	"4B	$tB	'�B	1AB	1[B	0UB	-�B	0oB	3�B	5�B	5tB	7�B	9�B	9�B	9�B	9�B	8�B	9�B	>�B	@ B	B�B	D�B	I�B	IB	J�B	M�B	N"B	N<B	SB	TB	T,B	S@B	QhB	V9B	XEB	Z�B	_pB	a|B	a|B	bhB	bhB	bhB	bhB	b�B	e�B	k�B	l�B	l�B	k�B	k�B	o�B	q�B	r�B	t�B	u�B	u�B	u�B	uB	vB	z�B	{B	}B	~BB	~]B	�AB	�GB	�aB	�SB	�?B	�_B	�EB	�EB	�tB	��B	�lB	�^B	�rB	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�&B	�B	�0B	�6B	�=B	�"B	�CB	�]B	�B	�OB	�UB	�AB	�-B	�GB	�aB	�hB	�nB	�ZB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�(B	�HB	�@B	�9B	�1B	�QB	�QB	�eB	�xB	�jB	��B	�B	�hB	�B	�B	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�.B
 B
 4B
 B
 4B
 B
;B
;B
UB
GB
GB
MB
9B
SB
?B
YB
SB
YB
KB
	lB

=B

rB

rB

=B

XB

XB

rB
	lB
	lB
	�B
dB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
 �B
 �B
 �B
!B
"�B
"�B
#B
"�B
#B
#�B
%B
%�B
%�B
%�B
%�B
&B
&B
&B
&B
'B
(
B
($B
)*B
)*B
)B
)B
*B
+B
+6B
,"B
,=B
,=B
-B
-CB
,=B
,WB
,WB
,=B
/5B
0;B
0;B
0;B
1AB
1AB
1vB
2|B
3hB
4nB
4nB
5tB
6`B
6`B
7�B
7�B
7fB
6zB
6�B
6zB
8lB
8lB
8lB
8�B
8�B
7�B
7�B
9�B
9rB
9rB
9�B
9�B
9�B
9�B
:^B
:xB
;B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
=�B
>�B
=�B
=�B
?�B
?�B
?�B
?�B
?�B
?�B
>�B
>�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
E�B
G�B
H�B
H�B
H�B
G�B
H�B
G�B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
MB
K�B
K�B
MB
MB
NB
PB
RB
Q�B
Q�B
RB
RB
R B
QB
Q4B
TB
TB
UB
UB
U2B
VB
W?B
W$B
XB
XB
X+B
X+B
XEB
X+B
YKB
Z7B
ZB
ZB
Z7B
ZB
Z7B
Z7B
Z7B
YeB
X_B
X_B
YKB
X_B
Y1B
ZQB
[=B
[WB
\)B
\CB
\]B
\CB
\CB
]dB
]IB
^OB
]dB
^jB
_;B
_;B
_VB
_VB
_VB
`vB
`\B
`vB
a|B
bhB
bNB
bhB
bhB
bNB
b�B
bhB
bhB
b�B
b�B
c�B
dtB
d�B
d�B
d�B
e�B
e�B
ezB
ezB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
h�B
h�B
i�B
iyB
i�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<z��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710101049022017101010490220171010104902201806221231302018062212313020180622123130201804050426522018040504265220180405042652  JA  ARFMdecpA19c                                                                20171001093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171001003509  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171001003511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171001003511  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171001003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171001003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171001003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171001003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171001003512  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171001003513                      G�O�G�O�G�O�                JA  ARUP                                                                        20171001005639                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171001153342  CV  JULD            G�O�G�O�F�Q�                JM  ARGQJMQC2.0                                                                 20171001153342  CV  JULD_LOCATION   G�O�G�O�F�Q�                JM  ARGQJMQC2.0                                                                 20171001153342  CV  LATITUDE        G�O�G�O�A�x�                JM  ARGQJMQC2.0                                                                 20171001153342  CV  LONGITUDE       G�O�G�O��%X                JM  ARCAJMQC2.0                                                                 20171010014902  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171010014902  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192652  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033130  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                