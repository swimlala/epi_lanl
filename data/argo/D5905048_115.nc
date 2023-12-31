CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-05-04T00:35:23Z creation;2017-05-04T00:35:26Z conversion to V3.1;2019-12-19T08:08:34Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20170504003523  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               sA   JA  I2_0577_115                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��z�� 1   @��DDD�@2�M:���d��t�j1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�(�@���A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/��C1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6��D7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��\D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D���D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�33A�-A�/A�-A��A�VA�VA�%A�A�  A���A���A���A��A��A��A��A��/A��
A���A�AжFAХ�AЛ�A�~�A�`BA�?}A��A��
A�ƨA��A�bAˇ+A�1Aʛ�A���Aɛ�A�S�A�9XA���AȲ-AȁA�t�A�p�AȃA�~�A�bNA�dZA�hsA�;dA���Aƕ�Aś�A�bA�=qA���A�z�A�^5A�^5A�O�A�5?A�+A�oA��A��/A��
A���A°!A�v�A�"�A���A��A�O�A�ZA��9A��hA���A�33A��7A��A�A���A���A���A�jA�Q�A���A��A�1A�/A��A��^A��wA�K�A���A�x�A�\)A�\)A��yA�E�A�dZA��A�v�A�l�A���A�t�A�%A��9A�I�A���A�hsA�A�&�A��yA���A��A�oA���A��A�JA�ZA��7A�PA~�A}G�A{7LAw"�AtJAq�7Ap  An�Am�AkƨAi�Ae�AdbNAcS�Ab�A^��A\��AW�hAV�+AV  AU�AS��AS�7ARȴAR�AQAOoAM��AL  AKK�AI�AG�^AG+AF�uAF�AEADJAB��AA�A@��A?;dA=��A< �A:�9A:n�A9�A7�#A5�;A4��A4I�A3��A1��A0  A.�9A,�/A+ƨA*�A*-A)�FA'�mA'33A&n�A%hsA%7LA$ffA"��A"n�A!A �`A 9XA��A&�A��A�A�+AdZA�Al�A�HA(�A��A+A�A=qAr�AA�A��AC�A�A�A
��A��A�A��A;dA  AO�AȴAz�A��A�AO�A �@���@�x�@���@�j@��P@��H@�`B@�z�@�@�Ĝ@�@�V@���@�;d@�@��@��@���@��@�@�"�@�E�@�{@���@�{@��@�"�@�t�@��
@㝲@�K�@�@�^@�=q@�?}@��m@�^5@݉7@�Ĝ@ۥ�@ڧ�@ى7@؃@�  @��@�n�@Չ7@�A�@�C�@��@ҧ�@��T@�p�@���@��m@���@�/@��;@�o@���@��@�j@�1@�|�@�n�@ļj@���@���@�&�@��u@�j@�S�@��@�-@�X@�V@��9@�j@��;@���@�t�@�o@�-@�G�@���@��u@�b@�33@��@�?}@�A�@�b@��w@�ȴ@���@��7@�?}@���@�bN@���@�
=@���@��T@�@��h@�7L@��`@��@�b@�1@���@�33@���@�$�@�@��#@���@�?}@���@�j@��@��P@�33@��@�n�@�$�@���@���@�V@�r�@��m@��@�t�@�K�@�K�@��R@���@���@���@�ff@�`B@�x�@���@��@�o@�
=@��\@���@�j@�1@�j@��u@�l�@�-@�V@��^@�/@�7L@��-@���@���@��h@�x�@�7L@���@��h@��@��h@�`B@��D@�bN@�(�@���@�1'@���@���@���@�(�@�ƨ@�S�@�C�@�33@��@�M�@���@��@��@�@���@��h@�x�@���@�bN@�  @��
@�|�@�\)@��@���@�v�@�^5@��-@�hs@��@��@�p�@�X@�hs@�hs@�X@�/@�V@� �@��@���@�33@�;d@�;d@��+@��+@���@���@���@��R@��!@���@�~�@��@���@�O�@���@���@��@�Ĝ@���@�r�@�Q�@�1'@�  @�ƨ@���@�K�@�"�@��y@���@�n�@�J@���@��@��h@�V@��j@��@��D@�bN@�Q�@�bN@�r�@�Q�@�b@���@��@�t�@�dZ@�"�@��@���@�M�@�@��@��@��7@�/@�V@��j@��@� �@�\)@��y@�M�@��^@�x�@�/@�%@�%@�Ĝ@��@�Q�@��@���@���@�33@�@��@��H@���@��!@��\@�^5@�{@���@���@�x�@��@�Ĝ@�j@�1@|�@~��@~��@~E�@}�-@}`B@}�@|�@|�j@{ƨ@{o@z~�@z-@y�^@yX@yG�@x�`@xr�@xb@w|�@wK�@w+@v�y@vv�@v@u�-@u/@t�j@t�@s33@r�H@r��@rM�@q�#@q��@qhs@q�@p��@p��@pbN@p �@o��@n��@nff@n$�@n{@n@m/@l�@lj@l�@kƨ@kdZ@jn�@i��@ihs@i�@h�9@hr�@hA�@hA�@h  @g�;@g��@g+@f�R@fff@f@e��@e��@e�@ep�@e`B@eO�@eO�@d�@d�@d��@dZ@d9X@c�F@c�@c33@co@b��@b�!@b�\@bM�@a��@a��@a�^@ahs@a&�@`��@`�u@`Q�@_�w@_\)@_+@^ȴ@^ff@]�@]�h@]�@\�@\Z@\1@[�F@[dZ@[o@Z�H@ZM�@Y��@YX@Y�@X�u@X �@X �@W�@W�w@W�@W��@W�P@Wl�@V�@Vv�@V5?@U�@U@UO�@T9X@S�m@S�F@St�@R��@R=q@Q��@Q�@Q��@Qhs@Q�@P��@PbN@Pb@O�;@O|�@O
=@N�@N��@NE�@M�@M��@MO�@M/@L�/@L�D@L9X@K��@K�
@KdZ@K@J��@J�\@J~�@JM�@I�@H�`@H��@Hr�@H1'@Hb@H  @G\)@G;d@G�@F�y@F��@FV@F5?@F@EO�@D�@D�D@Dj@DZ@DZ@DI�@D(�@D1@C�
@C��@CS�@CC�@C"�@C"�@Co@B�H@B��@B�!@B^5@A��@AG�@@�`@@��@@�9@@�u@@A�@?�w@?l�@?+@?+@>�y@>ȴ@>�R@>�R@>�R@>�R@>V@=�h@<��@<�j@<9X@;�
@;C�@:�H@:~�@:M�@:-@:�@9��@9G�@9%@8�`@8��@8�u@81'@7�;@7|�@7\)@7K�@7+@6��@6ȴ@6�+@6v�@65?@5�@5�T@5�-@5�-@5�@5`B@5O�@5?}@4��@4��@4�D@4z�@49X@3�m@3��@3dZ@3o@2�!@2^5@2=q@2=q@2-@2J@1��@1&�@1�@0��@0��@0r�@0 �@/��@/K�@.��@.��@.v�@.v�@.V@-�T@-��@-O�@-V@,�j@,I�@,1@,1@,1@+�m@+ƨ@+�@+C�@+"�@+o@+@*��@*M�@*-@*�@)�^@)7L@(�`@(��@(Ĝ@(�u@(Q�@(  @'�@'�P@'|�@'\)@'\)@'\)@'+@&$�@%��@%p�@%`B@%`B@%O�@%?}@%�@$�/@$��@$z�@$I�@#�
@#�F@#��@#S�@#o@"�!@"�\@"^5@"=q@!��@!��@!�7@ ��@ ��@ �u@ r�@ Q�@   @��@\)@\)@K�@��@��@�+@V@�T@�@?}@�@��@I�@1@S�@@�H@��@n�@-@�#@��@�7@hs@G�@G�@7L@7L@%@Ĝ@Q�@�;@�P@�@
=@
=@�y@ȴ@��@�+@$�@@�T@@��@�@?}@��@�@��@��@�D@Z@9X@�@��@�
@�F@�@S�@"�@o@"�@"�@�@�!@~�@M�@�@�@J@��@�@�7@%@%@%@%@��@Ĝ@�u@�@bN@ �@b@��@�P@\)@;d@��@�@��@�+@�+@ff@$�@�h@p�@`B@�@�@��@�@z�@(�@1@1@�m@��@t�@C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�1'A�33A�-A�/A�-A��A�VA�VA�%A�A�  A���A���A���A��A��A��A��A��/A��
A���A�AжFAХ�AЛ�A�~�A�`BA�?}A��A��
A�ƨA��A�bAˇ+A�1Aʛ�A���Aɛ�A�S�A�9XA���AȲ-AȁA�t�A�p�AȃA�~�A�bNA�dZA�hsA�;dA���Aƕ�Aś�A�bA�=qA���A�z�A�^5A�^5A�O�A�5?A�+A�oA��A��/A��
A���A°!A�v�A�"�A���A��A�O�A�ZA��9A��hA���A�33A��7A��A�A���A���A���A�jA�Q�A���A��A�1A�/A��A��^A��wA�K�A���A�x�A�\)A�\)A��yA�E�A�dZA��A�v�A�l�A���A�t�A�%A��9A�I�A���A�hsA�A�&�A��yA���A��A�oA���A��A�JA�ZA��7A�PA~�A}G�A{7LAw"�AtJAq�7Ap  An�Am�AkƨAi�Ae�AdbNAcS�Ab�A^��A\��AW�hAV�+AV  AU�AS��AS�7ARȴAR�AQAOoAM��AL  AKK�AI�AG�^AG+AF�uAF�AEADJAB��AA�A@��A?;dA=��A< �A:�9A:n�A9�A7�#A5�;A4��A4I�A3��A1��A0  A.�9A,�/A+ƨA*�A*-A)�FA'�mA'33A&n�A%hsA%7LA$ffA"��A"n�A!A �`A 9XA��A&�A��A�A�+AdZA�Al�A�HA(�A��A+A�A=qAr�AA�A��AC�A�A�A
��A��A�A��A;dA  AO�AȴAz�A��A�AO�A �@���@�x�@���@�j@��P@��H@�`B@�z�@�@�Ĝ@�@�V@���@�;d@�@��@��@���@��@�@�"�@�E�@�{@���@�{@��@�"�@�t�@��
@㝲@�K�@�@�^@�=q@�?}@��m@�^5@݉7@�Ĝ@ۥ�@ڧ�@ى7@؃@�  @��@�n�@Չ7@�A�@�C�@��@ҧ�@��T@�p�@���@��m@���@�/@��;@�o@���@��@�j@�1@�|�@�n�@ļj@���@���@�&�@��u@�j@�S�@��@�-@�X@�V@��9@�j@��;@���@�t�@�o@�-@�G�@���@��u@�b@�33@��@�?}@�A�@�b@��w@�ȴ@���@��7@�?}@���@�bN@���@�
=@���@��T@�@��h@�7L@��`@��@�b@�1@���@�33@���@�$�@�@��#@���@�?}@���@�j@��@��P@�33@��@�n�@�$�@���@���@�V@�r�@��m@��@�t�@�K�@�K�@��R@���@���@���@�ff@�`B@�x�@���@��@�o@�
=@��\@���@�j@�1@�j@��u@�l�@�-@�V@��^@�/@�7L@��-@���@���@��h@�x�@�7L@���@��h@��@��h@�`B@��D@�bN@�(�@���@�1'@���@���@���@�(�@�ƨ@�S�@�C�@�33@��@�M�@���@��@��@�@���@��h@�x�@���@�bN@�  @��
@�|�@�\)@��@���@�v�@�^5@��-@�hs@��@��@�p�@�X@�hs@�hs@�X@�/@�V@� �@��@���@�33@�;d@�;d@��+@��+@���@���@���@��R@��!@���@�~�@��@���@�O�@���@���@��@�Ĝ@���@�r�@�Q�@�1'@�  @�ƨ@���@�K�@�"�@��y@���@�n�@�J@���@��@��h@�V@��j@��@��D@�bN@�Q�@�bN@�r�@�Q�@�b@���@��@�t�@�dZ@�"�@��@���@�M�@�@��@��@��7@�/@�V@��j@��@� �@�\)@��y@�M�@��^@�x�@�/@�%@�%@�Ĝ@��@�Q�@��@���@���@�33@�@��@��H@���@��!@��\@�^5@�{@���@���@�x�@��@�Ĝ@�j@�1@|�@~��@~��@~E�@}�-@}`B@}�@|�@|�j@{ƨ@{o@z~�@z-@y�^@yX@yG�@x�`@xr�@xb@w|�@wK�@w+@v�y@vv�@v@u�-@u/@t�j@t�@s33@r�H@r��@rM�@q�#@q��@qhs@q�@p��@p��@pbN@p �@o��@n��@nff@n$�@n{@n@m/@l�@lj@l�@kƨ@kdZ@jn�@i��@ihs@i�@h�9@hr�@hA�@hA�@h  @g�;@g��@g+@f�R@fff@f@e��@e��@e�@ep�@e`B@eO�@eO�@d�@d�@d��@dZ@d9X@c�F@c�@c33@co@b��@b�!@b�\@bM�@a��@a��@a�^@ahs@a&�@`��@`�u@`Q�@_�w@_\)@_+@^ȴ@^ff@]�@]�h@]�@\�@\Z@\1@[�F@[dZ@[o@Z�H@ZM�@Y��@YX@Y�@X�u@X �@X �@W�@W�w@W�@W��@W�P@Wl�@V�@Vv�@V5?@U�@U@UO�@T9X@S�m@S�F@St�@R��@R=q@Q��@Q�@Q��@Qhs@Q�@P��@PbN@Pb@O�;@O|�@O
=@N�@N��@NE�@M�@M��@MO�@M/@L�/@L�D@L9X@K��@K�
@KdZ@K@J��@J�\@J~�@JM�@I�@H�`@H��@Hr�@H1'@Hb@H  @G\)@G;d@G�@F�y@F��@FV@F5?@F@EO�@D�@D�D@Dj@DZ@DZ@DI�@D(�@D1@C�
@C��@CS�@CC�@C"�@C"�@Co@B�H@B��@B�!@B^5@A��@AG�@@�`@@��@@�9@@�u@@A�@?�w@?l�@?+@?+@>�y@>ȴ@>�R@>�R@>�R@>�R@>V@=�h@<��@<�j@<9X@;�
@;C�@:�H@:~�@:M�@:-@:�@9��@9G�@9%@8�`@8��@8�u@81'@7�;@7|�@7\)@7K�@7+@6��@6ȴ@6�+@6v�@65?@5�@5�T@5�-@5�-@5�@5`B@5O�@5?}@4��@4��@4�D@4z�@49X@3�m@3��@3dZ@3o@2�!@2^5@2=q@2=q@2-@2J@1��@1&�@1�@0��@0��@0r�@0 �@/��@/K�@.��@.��@.v�@.v�@.V@-�T@-��@-O�@-V@,�j@,I�@,1@,1@,1@+�m@+ƨ@+�@+C�@+"�@+o@+@*��@*M�@*-@*�@)�^@)7L@(�`@(��@(Ĝ@(�u@(Q�@(  @'�@'�P@'|�@'\)@'\)@'\)@'+@&$�@%��@%p�@%`B@%`B@%O�@%?}@%�@$�/@$��@$z�@$I�@#�
@#�F@#��@#S�@#o@"�!@"�\@"^5@"=q@!��@!��@!�7@ ��@ ��@ �u@ r�@ Q�@   @��@\)@\)@K�@��@��@�+@V@�T@�@?}@�@��@I�@1@S�@@�H@��@n�@-@�#@��@�7@hs@G�@G�@7L@7L@%@Ĝ@Q�@�;@�P@�@
=@
=@�y@ȴ@��@�+@$�@@�T@@��@�@?}@��@�@��@��@�D@Z@9X@�@��@�
@�F@�@S�@"�@o@"�@"�@�@�!@~�@M�@�@�@J@��@�@�7@%@%@%@%@��@Ĝ@�u@�@bN@ �@b@��@�P@\)@;d@��@�@��@�+@�+@ff@$�@�h@p�@`B@�@�@��@�@z�@(�@1@1@�m@��@t�@C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
H�B
G�B
G�B
H�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
I�B
G�B
F�B
D�B
B�B
E�B
XB
�B
�B
�B
�9B
�FB
��B
ƨB
��B
�
B
�)B
�)B
�/B
�BB
�B
��B
��B
=BA�BgmBm�B�B��B�B�?B�-B�9B�9B�?B�RB�dB�jBB��B��B��B��B��B�#B�TB�B�B��BBPBuB�B�B#�B'�B/B33B5?B:^B6FB5?B49B/B'�B�B�B(�B>wBA�B:^B49B{B��B�HB��B�B�VBI�B1'B2-BM�BM�BYBXBT�BL�BH�B>wB1'B�B
��B
ŢB
��B
�JB
�=B
�B
dZB
S�B
K�B
B�B
6FB
�B
1B	��B	�B	�5B	�B	��B	�jB	��B	��B	�\B	�7B	y�B	k�B	J�B	B�B	>wB	;dB	5?B	1'B	-B	(�B	&�B	 �B	�B	hB	VB	VB	B��B��B��B��B�B�`B�BB��B��B��B�jB�LBB�}B�jB�9B�B�B�B��B��B��B��B��B�hB�\B�PB�DB�+B�%B�B�B� B|�Bz�Bx�Bw�Bu�Bs�Br�Bp�Bm�Bm�Bl�BjBhsBgmBe`BcTB`BB^5BZBS�BR�BQ�BQ�BP�BO�BM�BM�BH�BH�BJ�BJ�BK�BL�BL�BO�BQ�BT�BYBbNBbNBdZBffBffBffBgmBgmBk�Bk�Bo�Bs�By�B}�B{�B|�B�B�B�B�%B�B�1B�VB�{B��B��B��B��B�B�?B�FB�LB�}B��B��B��B�B�B�B�)B�/B�BB�TB�`B�yB�B�B�B�B�B�B��B��B��B��B	B	+B		7B	
=B	DB	DB	PB	PB	PB	VB	VB	\B	bB	bB	hB	oB	oB	�B	�B	�B	�B	!�B	%�B	&�B	&�B	(�B	)�B	,B	-B	,B	,B	,B	-B	0!B	49B	7LB	:^B	<jB	=qB	<jB	>wB	A�B	B�B	D�B	E�B	E�B	F�B	F�B	G�B	I�B	K�B	M�B	N�B	Q�B	R�B	W
B	YB	[#B	^5B	_;B	bNB	ffB	hsB	hsB	hsB	hsB	hsB	iyB	jB	m�B	p�B	s�B	t�B	w�B	|�B	� B	�B	�B	�B	�1B	�1B	�DB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�9B	�LB	�RB	�XB	�XB	�^B	�}B	�}B	�}B	�}B	��B	�}B	�}B	��B	��B	ƨB	��B	��B	��B	�B	�B	�B	�B	�#B	�B	�#B	�#B	�/B	�HB	�TB	�TB	�TB	�TB	�TB	�NB	�HB	�HB	�NB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B

=B

=B

=B

=B
DB

=B

=B

=B

=B
DB
DB

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
VB
\B
\B
bB
\B
bB
bB
bB
hB
oB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
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
+B
+B
,B
+B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
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
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
`BB
aHB
aHB
aHB
`BB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
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
gmB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
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
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
H�B
G�B
G�B
H�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
KB
KB
J	B
H1B
G�B
F�B
G�B
G_B
XyB
�3B
�B
�B
�ZB
�B
�'B
��B
�dB
׍B
�xB
�CB
�/B
�\B
�B
�B
�B
�BBBh�Bp;B��B��B�wB��B��B�nB�TB�tB��B�B��B��B��B��B�B�4BյB��B�&B�"B��B��B�B�B�B�B vB'�B,�B4B7B8�B;B7B7LB7�B2-B,qB �B�B+�BAUBC�B=�B9	B�B��B�B�aB��B�,BMB4�B3hBPbBN�BZBX�BVmBNpBKDBBB6+B vB
�(B
�#B
�xB
�B
��B
��B
ffB
U�B
NB
E�B
:�B
"NB
B	��B	�CB	�BB	ںB	�"B	��B	�B	�qB	��B	�6B	}VB	p;B	L0B	CaB	?}B	=B	6B	2GB	.IB	*�B	)_B	"�B	xB	�B	�B	HB	�B��B��B��B��B�!B��B�B��B��BðB�(B�lB�MB�UB��B��B�B�wB��B�B��B��B�B��B�oB�}B�vB�dB�fB�EB��B�aB��B}�B{�By�Bx�Bv�Bu?Bu�Br�Bn�Bo5Bn/BkQBi_BhsBffBd�Bc Ba�B\BT�BS�BS&BS�BRTBQ4BPBO�BJXBJ�BLdBK�BL�BMjBM�BP}BR�BVBZ�Bc�Bb�Bd�Bg8BgBg�BhsBi*BlqBlqBp�Bt�B{BB|�B}�B��B��B�B��B��B��B��B��B��B��B��B��B�WB��B�2B��B��B͟B�B��BּBּB�B��B�B�B��B�B�0B�CB��B�[B�B�3B�9B�FB�fB��B��B	AB	B		�B	B	�B	�B	�B	B	<B	�B	�B	.B	�B	�B	�B	&B	�B	SB	1B	�B	�B	"4B	&LB	'8B	'RB	)_B	*�B	,�B	-wB	,WB	,�B	,�B	-�B	0�B	4�B	7�B	:�B	="B	>(B	<�B	>�B	BB	B�B	E9B	F%B	F%B	G+B	F�B	G�B	J#B	L0B	N<B	OBB	R:B	S[B	WYB	YB	[�B	^jB	_pB	b�B	f�B	h�B	h�B	h�B	h�B	h�B	i�B	j�B	m�B	p�B	t9B	u%B	xRB	}<B	�OB	�aB	�MB	�mB	��B	�fB	�^B	��B	��B	� B	�oB	�gB	�EB	�WB	�$B	��B	��B	��B	�8B	��B	�>B	��B	��B	�B	��B	�]B	�B	��B	�B	�fB	��B	��B	��B	�^B	��B	��B	��B	��B	�B	��B	��B	��B	�oB	�YB	��B	�4B	�@B	�SB	�yB	�EB	�QB	یB	چB	�qB	�#B	�dB	�|B	�nB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�fB	�yB	�B	�B	�B	�B	��B	��B	�B	�lB	�B	��B	�FB	��B	�	B	�dB	��B	��B	��B	��B
 B
;B
AB
GB
{B
MB
mB
tB
_B
_B
_B
fB
	lB

XB

XB

rB

�B
xB

rB

�B

�B

rB
xB
xB

rB

rB

�B
�B
xB
xB
xB
xB
xB
JB
pB
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B

B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 'B
!B
 �B
 �B
 �B
!�B
!�B
# B
$&B
$B
%B
&B
&B
%�B
&2B
'8B
'B
'B
(>B
(>B
)_B
)*B
)*B
)*B
*KB
+6B
+6B
+6B
+B
,=B
+B
,"B
,WB
,WB
-CB
-)B
-)B
-)B
-wB
.cB
.IB
/5B
/iB
/iB
/iB
0oB
0UB
1[B
1AB
1[B
1[B
2-B
2aB
2aB
2aB
2|B
2|B
2aB
3hB
3MB
3MB
3MB
33B
3MB
33B
3MB
3hB
4TB
4nB
4TB
4TB
4nB
5ZB
5ZB
5tB
5tB
5tB
5tB
5ZB
5ZB
5tB
6`B
6zB
6`B
6`B
7�B
7fB
7�B
7�B
8lB
8�B
8�B
8�B
9�B
9�B
9�B
9�B
:xB
:�B
:�B
:xB
:�B
:�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
LB
K�B
L�B
L�B
MB
MB
MB
NB
M�B
M�B
NB
NB
M�B
M�B
M�B
NB
N"B
N<B
O(B
N�B
OB
P.B
P.B
Q B
QB
QB
QB
Q B
Q4B
RB
R B
R B
R B
RB
S&B
SB
S&B
SB
S�B
T,B
T,B
T,B
T,B
T,B
TB
T,B
T�B
U2B
UB
U2B
U2B
U2B
VB
V9B
V9B
VB
VB
V9B
V9B
VB
W?B
W?B
W$B
W$B
W?B
W
B
W?B
X+B
X_B
XEB
XEB
X+B
X+B
Y1B
YKB
YKB
YeB
Z7B
ZQB
[=B
[#B
[WB
[WB
[=B
[=B
\CB
\CB
\xB
\]B
]/B
]/B
]dB
]IB
]IB
]IB
]dB
]/B
]dB
]IB
^jB
^jB
^OB
^�B
^jB
_pB
_;B
_pB
_VB
_VB
`vB
`\B
abB
abB
`vB
abB
a|B
abB
`�B
b�B
b�B
bhB
bNB
bNB
b�B
cnB
cnB
cnB
c�B
cnB
c�B
dtB
dtB
d�B
d�B
dtB
ezB
e�B
e�B
e�B
f�B
f�B
f�B
g�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
k�B
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
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
uB
t�B
u�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
w�B
xB
w�B
xB
x�B
y	B
x�B
x�B
x�B
y�B
zB
zB
y�B
y�B
zB
y�B
{B
z�B
|1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201705080036442017050800364420170508003644201806221312572018062213125720180622131257201804050714212018040507142120180405071421  JA  ARFMdecpA19c                                                                20170504093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170504003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170504003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170504003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170504003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170504003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170504003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170504003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170504003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170504003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20170504010819                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170504153410  CV  JULD            G�O�G�O�F�%�                JM  ARCAJMQC2.0                                                                 20170507153644  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170507153644  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221421  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041257  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                