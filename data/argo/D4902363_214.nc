CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-28T00:35:23Z creation;2018-02-28T00:35:28Z conversion to V3.1;2019-12-19T07:48:19Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180228003523  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_214                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�O�f5� 1   @�O�'�} @:X���F�dg\(�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڃ3D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@�(�@�(�A{A>{A\z�A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=��C?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#��D$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D���D�8�D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�\Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�x�D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�bNA�\)A�XA�VA�S�A�S�A�O�A�;dA��A��;A���A�M�A�(�A���A�jA�;dA��A��A���A��A��A���A�$�A��A��A��mA���A���A�|�A�v�A�l�A�dZA�7LA�1A���A��wA���A�jA��A��A��A�bA��A���A��wA��9A��!A���A���A��A�ĜA�v�A�Q�A�1A���A���A���A��A�A�A�&�A��DA��A���A�bNA�VA�=qA�(�A���A��+A�Q�A�\)A�JA�+A��FA��A�VA��7A�XA���A��
A���A��
A�+A�ƨA��A�C�A\)A}�7Ay�AwoAu��At1'Ar��Aq�Aqp�Ap��Ap$�An�9Am��Al��AiƨAi
=Ah�Ah�DAh=qAh1Af�Ac&�Ab�+Ab=qA`��A_l�A]�A\1AZ�AYG�AX�jAX-AW��AV��AVM�AU�FAT�ATZAS�#AS|�AS+ARAQC�AQ�APv�AO�AN�!AMƨAL1AKoAI�^AG��AF��AF9XAE�^AD�jACVAB{A@�yA?�A?�A>��A>  A=?}A<��A<E�A:�A:$�A9p�A9XA9VA8��A8��A8$�A7\)A6�+A5�;A4��A3�TA3%A2�A0�A.Q�A-XA,bNA+��A+��A*�A*1'A(E�A'x�A'
=A&�uA&-A%�
A%S�A$ĜA$Q�A#�A#7LA"�`A"��A"��A!�
A!�A!33A�7AAȴA�Az�AE�A�Ap�AffA=qA1'AbAbA%AK�A�mA�Az�A-A��A|�AXA�A�AA�A"�AVA�
AO�AQ�A�`A�mAC�AoA
��A
��A	�mA��AC�A1'A{A�7A��A�hA\)A+A=qA33A ȴA 5?@��@���@�x�@�&�@��#@�@��@�@�C�@�ȴ@�-@�l�@��@��/@�$�@�bN@�1'@�`B@�l�@���@���@��/@ץ�@��H@�=q@�&�@�\)@ѩ�@�Ĝ@ϝ�@·+@͙�@��`@���@�r�@�(�@˶F@�+@��H@��@�1'@��@���@�t�@�5?@�hs@�%@��9@�9X@�t�@��\@�/@�b@��H@��!@���@��
@�C�@�ȴ@���@�X@���@��@��F@�S�@�ȴ@��h@��j@�r�@��@�K�@��@�n�@�-@���@�x�@�G�@�&�@���@��w@���@�l�@�;d@�
=@���@�=q@��D@��m@�S�@���@�v�@���@�%@��j@��u@�1'@��w@�+@�^5@�-@�J@���@��/@�  @��@�J@��#@�`B@�?}@�Ĝ@��u@��@�bN@��;@�o@�v�@��@�@�x�@�X@�?}@���@���@���@��m@��w@��w@��P@�?}@�z�@���@�~�@�p�@���@��@�1'@�  @���@�|�@�l�@�l�@�S�@�;d@�@���@��@���@��u@�A�@�t�@���@��@�E�@�{@�J@��@���@�O�@�?}@���@���@��u@�r�@�Z@�I�@�A�@���@��@�t�@�"�@���@��!@���@�n�@�E�@�-@��@���@��@��@���@�`B@�hs@��h@��-@���@�X@�&�@��@�r�@�1'@�(�@���@�^5@���@�X@��@��@���@���@���@���@��D@�Q�@��@~�+@~v�@~E�@}�@}�h@|I�@|�@{��@{�
@{�@{"�@{"�@y�^@x�9@x�u@x�@xr�@xQ�@xA�@x1'@w�;@w|�@v@t��@tI�@s��@s��@s�@st�@sdZ@sS�@sS�@sS�@sS�@r��@q��@q�^@q��@q��@q��@q%@pr�@pb@o�@pb@p �@pA�@pr�@pbN@p �@o�w@oK�@nV@m�-@m�h@m`B@m/@l�@l�j@k�m@k@j~�@j�!@j^5@j�@jJ@i��@iX@h�`@h��@hbN@hQ�@h1'@hb@g�@g�w@g;d@f�@f��@fff@f5?@f@e��@e�@d�@dj@d�@cƨ@ct�@c33@b�@b�\@b-@a�@a��@a�@`r�@`Q�@`b@_�@_+@_
=@^�y@^�@^��@^E�@]�-@]�h@]p�@]`B@]O�@]/@]V@\�@\��@\�D@\(�@[�m@[dZ@[o@Z��@Zn�@Z�@Y��@YX@X�9@XA�@Xb@X  @W�@W�;@W�w@W�@W�P@W\)@W;d@V�+@U@U`B@T��@T�D@TI�@S�
@S��@SS�@S"�@R��@R��@Rn�@RJ@Q�7@QX@QG�@Q&�@Q%@PĜ@O�;@OK�@N�R@N�+@NV@N5?@N@M�@M�@L�@L(�@K�@K@J��@J�\@J~�@J�@I�^@I�^@I��@H��@H�@HbN@HA�@H1'@H �@G�@G�;@G��@G|�@G;d@F�y@F��@Fv�@FE�@E�@E�h@Ep�@E`B@E?}@E?}@E�@EV@D��@D�@D�j@D��@Dz�@DI�@C��@Cƨ@Cƨ@C��@C�@CC�@B��@A�@A��@AG�@@�`@@�9@@r�@@bN@@Q�@@A�@?�@?�P@?l�@?\)@?;d@?+@>�@>�+@>V@>E�@>5?@=�@=�h@=O�@=V@<��@<�D@<Z@<�@;�m@;�
@;dZ@:�H@:n�@:-@9X@8�`@8�9@8bN@81'@8 �@8b@8 �@7�;@7�P@7l�@7K�@7+@7�@6�y@6V@6@5�@5�-@4�@4��@4Z@41@3�F@3�@3t�@3dZ@333@3"�@3o@3@2�@2�!@2=q@1�^@1�7@1hs@0�`@/|�@.�y@.��@.�+@.V@.E�@.$�@-�T@-`B@-�@,��@,�@,��@,j@,1@+�F@+dZ@+"�@*��@*n�@)�^@)x�@)hs@)7L@(��@(bN@'�@'\)@'+@'
=@&�y@&�y@&�@&��@&$�@%��@%?}@%/@%V@$�@$��@#�m@#��@#C�@#33@"�@"��@"n�@"M�@"=q@"�@!�@!�7@!G�@ ��@ �u@ bN@  �@�@�P@|�@;d@�@��@�R@��@@{@�@@�h@�@z�@�@�j@�j@�j@I�@t�@S�@33@�@�@�H@�H@��@��@~�@n�@n�@��@��@��@�7@�7@x�@X@G�@G�@G�@7L@�@�`@�`@�`@�`@��@�u@�@Q�@�w@;d@V@$�@�T@��@@�-@�-@�-@��@��@�h@`B@O�@?}@��@j@Z@I�@9X@Z@j@I�@�F@�@dZ@o@@��@�\@n�@M�@~�@n�@n�@-@-@�@�@�@��@��@��@x�@G�@��@�u@bN@1'@�@��@��@��@��@��@�@�P@K�@;d@��@��@V@$�@$�@$�@��@��@?}@�@�/@�@��@z�@Z@�@�m@�m@�
@�
@��@��@t�@@
�!@
M�@
�@	��@	�#@	��@	�^@	��@	hs@	�@��@�`@Ĝ@��@�@bN@ �@�@��@l�@l�@\)@+@�@
=@��@�y@ȴ@�+@V@5?@5?@5?@5?@$�@{@�@��@��@�@p�@O�@/@��@��@Z@I�@I�@�@�@1@1@ƨ@�@dZ@C�@"�@"�@o@@�H@�\@n�@M�@=q@-@�@�#@��@�^@��@x�@x�111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�bNA�\)A�XA�VA�S�A�S�A�O�A�;dA��A��;A���A�M�A�(�A���A�jA�;dA��A��A���A��A��A���A�$�A��A��A��mA���A���A�|�A�v�A�l�A�dZA�7LA�1A���A��wA���A�jA��A��A��A�bA��A���A��wA��9A��!A���A���A��A�ĜA�v�A�Q�A�1A���A���A���A��G�O�G�O�A��DA��A���A�bNA�VA�=qA�(�A���A��+A�Q�A�\)A�JA�+A��FA��A�VA��7A�XA���A��
A���A��
A�+A�ƨA��A�C�A\)A}�7Ay�AwoAu��At1'Ar��Aq�Aqp�Ap��Ap$�An�9Am��Al��AiƨAi
=Ah�Ah�DAh=qAh1Af�Ac&�Ab�+Ab=qA`��A_l�A]�A\1AZ�AYG�AX�jAX-AW��AV��AVM�AU�FAT�ATZAS�#AS|�AS+ARAQC�AQ�APv�AO�AN�!AMƨAL1AKoAI�^AG��AF��AF9XAE�^AD�jACVAB{A@�yA?�A?�A>��A>  A=?}A<��A<E�A:�A:$�A9p�A9XA9VA8��A8��A8$�A7\)A6�+A5�;A4��A3�TA3%A2�A0�A.Q�A-XA,bNA+��A+��A*�A*1'A(E�A'x�A'
=A&�uA&-A%�
A%S�A$ĜA$Q�A#�A#7LA"�`A"��A"��A!�
A!�A!33A�7AAȴA�Az�AE�A�Ap�AffA=qA1'AbAbA%AK�A�mA�Az�A-A��A|�AXA�A�AA�A"�AVA�
AO�AQ�A�`A�mAC�AoA
��A
��A	�mA��AC�A1'A{A�7A��A�hA\)A+A=qA33A ȴA 5?@��@���@�x�@�&�@��#@�@��@�@�C�@�ȴ@�-@�l�@��@��/@�$�@�bN@�1'@�`B@�l�@���@���@��/@ץ�@��H@�=q@�&�@�\)@ѩ�@�Ĝ@ϝ�@·+@͙�@��`@���@�r�@�(�@˶F@�+@��H@��@�1'@��@���@�t�@�5?@�hs@�%@��9@�9X@�t�@��\@�/@�b@��H@��!@���@��
@�C�@�ȴ@���@�X@���@��@��F@�S�@�ȴ@��h@��j@�r�@��@�K�@��@�n�@�-@���@�x�@�G�@�&�@���@��w@���@�l�@�;d@�
=@���@�=q@��D@��m@�S�@���@�v�@���@�%@��j@��u@�1'@��w@�+@�^5@�-@�J@���@��/@�  @��@�J@��#@�`B@�?}@�Ĝ@��u@��@�bN@��;@�o@�v�@��@�@�x�@�X@�?}@���@���@���@��m@��w@��w@��P@�?}@�z�@���@�~�@�p�@���@��@�1'@�  @���@�|�@�l�@�l�@�S�@�;d@�@���@��@���@��u@�A�@�t�@���@��@�E�@�{@�J@��@���@�O�@�?}@���@���@��u@�r�@�Z@�I�@�A�@���@��@�t�@�"�@���@��!@���@�n�@�E�@�-@��@���@��@��@���@�`B@�hs@��h@��-@���@�X@�&�@��@�r�@�1'@�(�@���@�^5@���@�X@��@��@���@���@���@���@��D@�Q�@��@~�+@~v�@~E�@}�@}�h@|I�@|�@{��@{�
@{�@{"�@{"�@y�^@x�9@x�u@x�@xr�@xQ�@xA�@x1'@w�;@w|�@v@t��@tI�@s��@s��@s�@st�@sdZ@sS�@sS�@sS�@sS�@r��@q��@q�^@q��@q��@q��@q%@pr�@pb@o�@pb@p �@pA�@pr�@pbN@p �@o�w@oK�@nV@m�-@m�h@m`B@m/@l�@l�j@k�m@k@j~�@j�!@j^5@j�@jJ@i��@iX@h�`@h��@hbN@hQ�@h1'@hb@g�@g�w@g;d@f�@f��@fff@f5?@f@e��@e�@d�@dj@d�@cƨ@ct�@c33@b�@b�\@b-@a�@a��@a�@`r�@`Q�@`b@_�@_+@_
=@^�y@^�@^��@^E�@]�-@]�h@]p�@]`B@]O�@]/@]V@\�@\��@\�D@\(�@[�m@[dZ@[o@Z��@Zn�@Z�@Y��@YX@X�9@XA�@Xb@X  @W�@W�;@W�w@W�@W�P@W\)@W;d@V�+@U@U`B@T��@T�D@TI�@S�
@S��@SS�@S"�@R��@R��@Rn�@RJ@Q�7@QX@QG�@Q&�@Q%@PĜ@O�;@OK�@N�R@N�+@NV@N5?@N@M�@M�@L�@L(�@K�@K@J��@J�\@J~�@J�@I�^@I�^@I��@H��@H�@HbN@HA�@H1'@H �@G�@G�;@G��@G|�@G;d@F�y@F��@Fv�@FE�@E�@E�h@Ep�@E`B@E?}@E?}@E�@EV@D��@D�@D�j@D��@Dz�@DI�@C��@Cƨ@Cƨ@C��@C�@CC�@B��@A�@A��@AG�@@�`@@�9@@r�@@bN@@Q�@@A�@?�@?�P@?l�@?\)@?;d@?+@>�@>�+@>V@>E�@>5?@=�@=�h@=O�@=V@<��@<�D@<Z@<�@;�m@;�
@;dZ@:�H@:n�@:-@9X@8�`@8�9@8bN@81'@8 �@8b@8 �@7�;@7�P@7l�@7K�@7+@7�@6�y@6V@6@5�@5�-@4�@4��@4Z@41@3�F@3�@3t�@3dZ@333@3"�@3o@3@2�@2�!@2=q@1�^@1�7@1hs@0�`@/|�@.�y@.��@.�+@.V@.E�@.$�@-�T@-`B@-�@,��@,�@,��@,j@,1@+�F@+dZ@+"�@*��@*n�@)�^@)x�@)hs@)7L@(��@(bN@'�@'\)@'+@'
=@&�y@&�y@&�@&��@&$�@%��@%?}@%/@%V@$�@$��@#�m@#��@#C�@#33@"�@"��@"n�@"M�@"=q@"�@!�@!�7@!G�@ ��@ �u@ bN@  �@�@�P@|�@;d@�@��@�R@��@@{@�@@�h@�@z�@�@�j@�j@�j@I�@t�@S�@33@�@�@�H@�H@��@��@~�@n�@n�@��@��@��@�7@�7@x�@X@G�@G�@G�@7L@�@�`@�`@�`@�`@��@�u@�@Q�@�w@;d@V@$�@�T@��@@�-@�-@�-@��@��@�h@`B@O�@?}@��@j@Z@I�@9X@Z@j@I�@�F@�@dZ@o@@��@�\@n�@M�@~�@n�@n�@-@-@�@�@�@��@��@��@x�@G�@��@�u@bN@1'@�@��@��@��@��@��@�@�P@K�@;d@��@��@V@$�@$�@$�@��@��@?}@�@�/@�@��@z�@Z@�@�m@�m@�
@�
@��@��@t�@@
�!@
M�@
�@	��@	�#@	��@	�^@	��@	hs@	�@��@�`@Ĝ@��@�@bN@ �@�@��@l�@l�@\)@+@�@
=@��@�y@ȴ@�+@V@5?@5?@5?@5?@$�@{@�@��@��@�@p�@O�@/@��@��@Z@I�@I�@�@�@1@1@ƨ@�@dZ@C�@"�@"�@o@@�H@�\@n�@M�@=q@-@�@�#@��@�^@��@x�@x�111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BS�B� B��B��B�B�3B�dB�wBB�XB�!B��B�+B�FB��B��B��B��B��B��B��B��B�wB�!B�dBĜBȴBĜBƨB��B��B��BȴBǮBȴBɺBƨBB�dB�^B�B��B�B�B�B�B�B��B��B��B�JB�DB�bB�+Bx�BcTBE�B1'B�B�?BDB-BE�BN�BR�BM�BE�B2-BhB
��B
��B
�/B
�
B
�B
ƨB
��B
ĜB
ÖB
�}B
�^B
�B
��B
|�B
cTB
T�B
ZB
E�B
,B	��B
B
	7B
B	��B	��B	��B	��B	�yB	�B	��B	��B	�B	ɺB	��B	��B	ȴB	��B	��B	{�B	��B	��B	�JB	|�B	u�B	l�B	jB	iyB	o�B	o�B	k�B	dZB	ffB	dZB	aHB	bNB	^5B	_;B	ZB	K�B	H�B	Q�B	A�B	1'B	9XB	+B	�B	�B	{B	B	
=B	�B	\B	B�B��B�B�B�B��B�B�B�B�sB�B�BB�`B�B�B�B�`B�B��B��B��B�qB�qB�XB�B��B�VB��B��B�B�B��B��B�+B�oB��B��B��B��B�uB�JB�\B�VB�+B�JB�\B�DB~�B� B{�BgmBx�B�B�B~�B{�Bu�Bp�BgmBu�Bx�Bs�BjBZB>wBL�BT�B\)B`BB_;B_;B`BB\)BT�BH�B5?B8RB�B:^B0!B)�B49B9XBE�BB�B=qB+B�B!�B!�B5?B+B�B33B5?B0!B$�B�B+B'�B(�B �B�B��BBPBoB�B{B�B�BB
=BJB��B��B�sB��BB{BhBhBhB�B�BbB	7BJB�B�B�B�B�B(�B%�B$�B"�B�B�B�BDBuB�B	7B�B#�B+B)�B&�B!�B �B�B!�B"�B-B%�B�B0!B1'B-B49B6FB0!B:^B8RB49B0!B5?B=qB<jB<jBA�BA�BC�BC�BD�BF�BF�BB�BA�BK�BK�BJ�BJ�BG�BC�B;dBK�BP�BR�BR�BP�BT�B[#B]/B[#B[#B[#B\)Be`BdZBaHB]/B_;BdZBe`Bt�Br�Bv�Bu�B{�B~�B|�By�By�B�B�1B�=B�VB�bB�hB�\B�hB�hB�bB��B��B�uB�1B��B��B��B��B�'B�RB�^B�jB�jB��B��BB��B��B�wB�dB�LBÖBŢBȴB��B��B�/B�B�NB�`B�`B�TB�`B�yB�sB�yB�B�B�B�B�B�B�B�B�B��B��B��B	  B	B	B	B	+B	DB	JB	DB	JB	{B	�B	�B	�B	�B	�B	�B	�B	$�B	&�B	 �B	�B	'�B	5?B	9XB	?}B	A�B	C�B	D�B	H�B	H�B	G�B	G�B	L�B	W
B	YB	YB	ZB	XB	cTB	dZB	e`B	dZB	dZB	gmB	dZB	gmB	p�B	q�B	q�B	q�B	q�B	q�B	o�B	n�B	k�B	o�B	x�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�%B	�B	�%B	�VB	�bB	�hB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�3B	�9B	�9B	�LB	�FB	�?B	�FB	�RB	�XB	�dB	�jB	�jB	�jB	�jB	�jB	��B	��B	B	B	ÖB	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�/B	�5B	�/B	�5B	�5B	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�TB	�TB	�ZB	�ZB	�fB	�fB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
B
B
B
1B
1B
	7B
	7B
	7B
	7B
	7B
1B
1B
1B

=B
DB
DB
DB
JB
VB
\B
bB
bB
bB
bB
hB
bB
bB
hB
hB
hB
hB
uB
{B
uB
oB
hB
bB
\B
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
#�B
#�B
#�B
"�B
"�B
%�B
%�B
%�B
%�B
$�B
#�B
%�B
&�B
%�B
$�B
(�B
)�B
)�B
+B
-B
-B
-B
-B
.B
.B
.B
-B
,B
+B
-B
.B
.B
,B
(�B
0!B
33B
49B
5?B
5?B
5?B
49B
33B
5?B
6FB
7LB
7LB
6FB
5?B
7LB
6FB
7LB
6FB
7LB
6FB
9XB
;dB
:^B
9XB
8RB
9XB
:^B
=qB
>wB
>wB
?}B
>wB
<jB
<jB
=qB
?}B
A�B
A�B
A�B
?}B
>wB
B�B
C�B
E�B
D�B
E�B
E�B
F�B
G�B
F�B
E�B
D�B
E�B
E�B
G�B
H�B
G�B
H�B
G�B
I�B
I�B
J�B
J�B
I�B
G�B
L�B
O�B
M�B
L�B
K�B
J�B
J�B
P�B
O�B
N�B
M�B
K�B
J�B
P�B
P�B
O�B
P�B
Q�B
P�B
P�B
O�B
O�B
O�B
O�B
N�B
P�B
R�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
R�B
S�B
S�B
R�B
R�B
R�B
R�B
P�B
M�B
N�B
M�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
R�B
S�B
VB
W
B
W
B
XB
YB
XB
VB
W
B
XB
W
B
XB
XB
W
B
YB
YB
[#B
[#B
ZB
ZB
\)B
\)B
[#B
[#B
[#B
\)B
\)B
\)B
[#B
[#B
[#B
]/B
\)B
]/B
]/B
^5B
_;B
^5B
_;B
^5B
^5B
]/B
^5B
]/B
\)B
]/B
^5B
`BB
_;B
^5B
_;B
^5B
_;B
aHB
aHB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
cTB
dZB
cTB
bNB
cTB
cTB
e`B
e`B
e`B
ffB
ffB
e`B
dZB
dZB
e`B
ffB
ffB
ffB
ffB
e`B
e`B
dZB
ffB
ffB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
hsB
iyB
iyB
iyB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
hsB
iyB
hsB
k�B
m�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
o�B
p�B
p�B
q�111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BU�B�B��B��B�B�3B��B��B��B�*B�vB��B��B�2B�dB�\B͟B�(B��B�B�B�B��B�[B�6B�B��B�B�B��B��B�B�7B�B�B�	B�B�B�B��B�5B��B�wB�IB�CB�/B�CB�B�B�BB��B��B��B�BzBezBH�B4TB�DG�O�G�O�B2BGEBOvBS&BN"BF?B3�B{B
��B
�B
�BB
�B
�B
�B
�B
ŢB
�MB
�iB
��B
�!B
��B
.B
f�B
WYB
[�B
G�B
.�B	�B
�B

�B
B	�B
  B	��B	��B	�B	��B	�[B	̘B	�B	�rB	�BB	�.B	�RB	�UB	��B	�OB	�vB	��B	�pB	~�B	xB	n�B	lWB	kB	poB	p�B	lqB	ezB	gB	e,B	bNB	cB	^�B	_�B	Z�B	MB	I�B	R B	B�B	2�B	:B	,�B	�B	;B	mB	�B	�B	B	bB	�B��B�+B�AB�B��B��B�B�wB�iB�_B��B�HB�LB��B�B�B��B�B�B�B��B��B��B��B��B��B�hB��B�2B��B��B��B��B��B�uB�QB�7B�?B�EB�FB�PB��B��B�B��B��B��B�B��B|�Bi�By�B�uB�GBcB|PBv`Bq�Bh�BvBy	BtBkB[�BA;BN�BVB]B`�B_�B_�B`�B\�BU�BI�B7B9�BB;B1�B+�B5tB:DBE�BB�B=�B,�B�B#nB# B5tB,B�B3�B5�B0�B&2B�B+�B(�B)�B!�B�BB%B�B�BSB�BBSB�BDB6B��B��B�QB��B?B�B B BTBEB9BhB
�B�B?B�B_BjBVB)DB&LB%,B#TB \B BByB�BaB�B
�BkB$ZB+kB*eB'�B"�B!�B�B"�B#�B-wB&�BB0�B1�B-�B4�B6�B1B:�B8�B4�B1'B5�B=�B=B=BA�BBBC�BC�BD�BF�BF�BCBB'BK�BK�BKBKBHBDMB<�BLdBQhBSuBS�BQ�BU�B[WB]~B[�B[�B[�B\�BezBd�Ba�B^B_�Be,BfBt�Bs3Bv�BvFB|B.B}<BzxBz�B��B��B��B��B��B��B��B��B��B� B��B��B��B��B�=B�~B��B��B��B��B��B��B��B��B��BªB��B��B��B��B�8B��B�%B�7B�jB�uB�dBںB�B�zB�B�B��B�B��B��B�B��B��B��B��B��B�B�B�B�+B�"B�.B	 4B	GB	9B	SB	EB	^B	dB	�B	�B	{B	�B	�B	�B	�B	�B	�B	/B	%,B	'B	!|B	 �B	(sB	5�B	9�B	?�B	A�B	C�B	D�B	H�B	H�B	HB	H1B	MPB	W
B	YKB	YKB	ZQB	X�B	cnB	dtB	e�B	d�B	d�B	g�B	d�B	g�B	p�B	q�B	q�B	q�B	q�B	q�B	o�B	o B	l=B	p!B	x�B	|B	~B	�B	�'B	�GB	�MB	�9B	�?B	�YB	��B	�tB	�pB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�BB	�B	�B	�B	�=B	�=B	�=B	�yB	�eB	�iB	�MB	�TB	�nB	�LB	�zB	�tB	��B	��B	�rB	�B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	� B	�,B	�@B	�&B	�FB	�EB	�EB	�+B	�SB	�IB	�dB	�OB	�dB	ބB	ބB	�B	�B	�B	�B	�nB	�nB	�B	�tB	�B	�B	�tB	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�B	�B	��B	�B	�B	�B	�*B	�2B	�B	�B	�.B
 B
 B
 B	�.B	�<B	�PB	�BB	�]B
UB
AB
SB
SB
aB
GB
?B
9B
aB
mB
KB
fB
	RB
	RB
	lB
	RB
	lB
KB
fB
�B

XB
xB
xB
xB
~B
pB
vB
bB
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 �B
 �B
#B
#�B
#�B
#�B
#B
"�B
&B
&B
%�B
%�B
%B
$B
%�B
'B
&2B
%FB
)*B
*0B
*B
+6B
-CB
-B
-CB
-)B
./B
.B
./B
-)B
,"B
+6B
-]B
.IB
.IB
,qB
)�B
0UB
3hB
4TB
5ZB
5ZB
5tB
4TB
3hB
5ZB
6zB
7�B
7�B
6zB
5�B
7�B
6`B
7fB
6zB
7fB
6�B
9�B
;dB
:�B
9�B
8�B
9�B
:�B
=�B
>�B
>�B
?}B
>�B
<�B
<�B
=�B
?�B
A�B
A�B
A�B
?�B
>�B
B�B
C�B
E�B
D�B
E�B
E�B
F�B
G�B
F�B
E�B
D�B
E�B
E�B
G�B
H�B
G�B
H�B
G�B
I�B
I�B
J�B
J�B
I�B
G�B
L�B
O�B
NB
MB
K�B
J�B
J�B
P�B
O�B
N�B
NB
K�B
KB
P�B
Q B
PB
P�B
RB
Q B
P�B
PB
PB
O�B
PB
O(B
Q B
SB
R B
R�B
SB
SB
S�B
TB
TB
TB
T,B
SB
S�B
TB
SB
S&B
SB
SB
QB
N"B
O(B
N"B
R�B
T,B
UB
UB
UB
T�B
UB
UB
T�B
UB
T,B
T,B
TB
S@B
T,B
VB
W$B
W
B
X+B
YB
X+B
VSB
W?B
XEB
W$B
X+B
XEB
W?B
Y1B
Y1B
[=B
[#B
Z7B
ZQB
\)B
\CB
[=B
[WB
[WB
\]B
\CB
\]B
[WB
[WB
[WB
]IB
\CB
]IB
]dB
^OB
_VB
^OB
_;B
^OB
^jB
]IB
^jB
]IB
\CB
]IB
^jB
`BB
_;B
^OB
_pB
^�B
_pB
abB
a|B
bNB
b�B
bhB
b�B
cnB
dtB
dtB
dZB
cnB
dZB
c�B
b�B
cnB
c�B
ezB
e�B
e�B
f�B
ffB
ezB
d�B
d�B
e�B
ffB
f�B
f�B
f�B
e�B
ezB
d�B
f�B
f�B
g�B
g�B
f�B
g�B
gmB
g�B
g�B
g�B
f�B
g�B
h�B
iyB
iyB
i�B
hsB
h�B
h�B
h�B
h�B
i�B
iyB
i�B
i�B
h�B
i�B
h�B
k�B
m�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
o�B
p�B
p�B
q�111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803040039432018030400394320180304003943201806221238202018062212382020180622123820201804050435172018040504351720180405043517  JA  ARFMdecpA19c                                                                20180228093522  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180228003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180228003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180228003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180228003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180228003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180228003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180228003527  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180228003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180228003527  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180228003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180228003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20180228005530                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180228153230  CV  JULD            G�O�G�O�F�}�                JM  ARCAJMQC2.0                                                                 20180303153943  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180303153943  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193517  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033820  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                