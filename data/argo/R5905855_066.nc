CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:22:15Z creation;2022-06-04T19:22:15Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192215  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               BA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�KU��71   @�KV�P�@@0wKƧ��c�V�u1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B���B���B�  B�  B�  BǙ�B�33Bϙ�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C9��C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@xQ�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�\)B��\B�\)B�B�B�B�B�B�\)B�\)B�B�B�B�\)B���B�\)Bӏ\B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7��C9�C;ǮC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCMǮCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]��C_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�?\D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��vA���A��[A��&A��HA��A���A�֡A�ޞA��ZA��A��;A���A���A���A���A��A��,A��fA��fA���A��sA��yA��KA��A��QA���A���A���A��/A��TA��A�چA��A��A��&A��.A�)_AЩ_A�~]A�{JAƹ$A��A�>�A��A��MA�l�A��A�ƨA��{A�e,A�ȴA�GA��A�+A�oA�q�A�ÖA��fA��~A�S&A�ĜA��A�~�A�cTA���A��A���A��A�'RA���A���A�cA��vA���A�xA��A�;�A�`A��PA��IA�>�A��VA��)A�-A��CA��2A���A��A��mA��A��A�~A�M�A�h�A�>A~�Ay4Av��AuK�As��Ar��Ao�oAhGEAf�Ac�AaGEA_f�A^��AY��AQ�4AN�OAN{AL��AJ�AH	AG��AF�DAE��AD�WAD}�AC�@AB��ABW�A@xA@�A?��A<˒A:ѷA9�eA8��A7�SA74A6�;A6��A6~�A5�MA4��A4&�A3��A36A2I�A/�A-�@A+MA(��A'�VA'A&��A&oiA&	A%IRA$��A$m]A$+A#�RA"�AA"(�A!�8A!�0A!r�A!A�A �A��A]dAc�A�AFAu�A��ADgA��A�7A��Ar�A�!A�zA�eA��A�|A��A�A{A��A�>AAAp�A͟A�A/A�A��AjA/�A(�A"�AeA�AaA�8A�wA��AzxA�A�A�4A�A�ADgAیAɆA|A�]Aw2A9XA�!A��A2aA�A��A��A@OA	�A
ɆA
v`A
�A	��A	H�A		A��A��A��A��AQA8�A�A�AaA*0A��A��AH�A:*A�A��A��A�An/Aa|AOvA6zA%�A�A��A�rAA�ZAƨA�~A@�A�^A�'A��AjAJA �A ��A �	A jA R�A V@���@��@�m�@���@���@�5�@���@�Z@�)�@��K@�hs@�m�@���@�c�@��@��@�M�@�hs@��@��@��F@�M@��@��@�	�@��@�*0@�4@���@��P@�@�@�ߤ@�ԕ@�o @�҉@�tT@�	@�=@�s�@�@@��M@涮@�1�@�~@�X@��@�n@���@◍@�<�@���@�(�@��+@�H@���@�P�@�֡@�J@ݜ�@���@܉�@�~@۬q@�c@�rG@�@O@�҉@ڍ�@��@ٓ@�dZ@ظR@�S�@�w2@���@��)@�Z�@���@՜�@��/@�x@Ӫ�@�t�@�c�@�1�@��@�ѷ@Ҏ�@�C�@���@�|�@���@�֡@���@�kQ@�<�@���@��Q@Ϻ^@���@�=q@�J@͓�@�K�@���@��@̇+@�3�@�
�@��@ʁo@ɭC@�1'@�l�@�Q�@��@ƌ�@�(�@�ԕ@�w2@��y@Đ.@��]@�X@³h@���@��X@���@��H@��K@�v�@��@��@���@�;d@� i@���@�*�@�hs@��@��_@�\�@��r@�A @���@�g8@�2�@��>@��V@�e,@��@���@��@�c�@�[�@�l�@�c @�Q�@�H@�B[@�Ft@��&@�N<@���@�w�@�Ov@���@�@�{J@�ں@�g8@�7@���@��	@�N<@�!-@���@��s@���@��$@�M@�G@��-@�\�@�+@��@�d�@��@���@�iD@�@@��[@�Ĝ@���@�_@���@�Vm@�0�@��@�֡@��@�i�@�!@���@�Mj@��@��9@��@���@�K�@�@@��]@���@��8@��@���@�w�@�u@���@�<6@��@�-�@���@�f�@�P�@�1�@��@��@��s@���@��h@��O@���@��L@�tT@���@��|@��1@�y>@�Q�@��@�zx@� i@���@���@��@��o@�W�@��@���@��$@�J#@�*0@�͟@���@�>B@��S@�x@�hs@�Vm@�)_@�҉@�y>@�J�@�'R@��z@�{J@�33@���@��]@�͟@��j@���@�d�@�H�@�@���@�j�@�Mj@�+@��@���@�c�@�:�@�3�@�O@��@�u�@�$t@��@�L0@��]@��@��~@�RT@�q@�V@�Ĝ@�I�@��.@��j@���@�IR@�͟@���@�tT@�W�@�1'@��Z@�rG@��@�$@���@��@�J�@���@�n�@��@�u@��@��d@�j�@�,�@��@��R@�*�@��@��@���@�c@�f�@�S�@�A @��M@�a|@��&@���@���@�)_@��@��5@��@��+@�U2@��@���@�_p@�%@��@��v@���@�R�@�G@�X@��K@���@�u%@�9X@���@�(�@��@�bN@�.�@�V@�@~�b@}�=@}|@}x�@}w2@}#�@|��@|r�@|PH@|'R@|b@{��@{��@{��@{�@{O@{�@z�X@zc @y�@y	l@w�]@w�@@wiD@w/�@v��@u��@u�)@uX@u�@t�v@t��@s�W@sP�@sY@r�c@r��@rV@r@q�)@q��@q��@oݘ@o�@n͟@n��@nE�@n@m�H@l�P@l�4@lV�@l�@l�@k�
@k��@k�:@k@O@kS@jں@jTa@i�@i��@iB�@h�@g~�@f��@f�+@e�z@dFt@c�f@b�@bi�@a�o@a�@aF@`ی@`�o@`?�@_ƨ@_��@_E9@^͟@^n�@]�d@]�h@]��@]%F@\ѷ@\_@\9X@\x@[�F@[H�@Zȴ@ZL0@Y�@Y��@YS&@Y4@Y�@Xm�@W�K@W��@Wx@V��@V��@VQ@VB[@V3�@U�@UJ�@U%F@U�@U@TɆ@T7@S�k@SJ#@R�s@RR�@Q��@Q�>@Q�@Q�9@Q�9@Q�@Q��@QIR@Q%F@P��@PV�@P4n@O�
@O�@N�m@Nv�@NV@N1�@N�@M��@MVm@MA @M�@M�@L�@Lw�@L:�@LM@L	�@K��@K�P@J�@J@IVm@I�@H�I@H!@G��@G��@Gg�@G;d@F��@E�@Em]@E#�@D�?@D~(@D@Cx@C)_@Bxl@A�@A@@@�)@@b@?� @?��@?iD@?9�@>�m@>{@=�^@=a�@<�f@<�.@<%�@;��@;W?@;"�@:�@:?@:{@9��@9�S@9}�@9X@9&�@8�j@8�@8e�@8'R@7�a@7{J@7e�@7J#@6��@6V@6Q@6M�@5��@5s�@55�@4�|@4��@4��@4�/@4�j@4��@4:�@3��@3�P@3"�@2҉@2��@2s�@2n�@2($@1�9@1��@1|@1Vm@0�@0��@0tT@0]d@0C-@/�@/g�@/�@.�@.�X@.��@.�@.�b@.��@.��@.4@-:�@-&�@-&�@-�@,��@,z�@,@+��@+t�@+�@*��@*�x@*��@*q�@*@�@*1�@)ϫ@)w2@)Y�@)Y�@)J�@)<6@)+@)V@)+@)+@(��@(��@(V�@'��@'�@'�*@'��@'��@'�@&�2@&ߤ@&�m@&u%@&@%ԕ@%�3@%�H@%�t@%�C@%�X@%�=@%=�@$��@#�A@#4�@"�@"��@"J�@"0U@!�.@!�H@!k�@!@@ ��@ Xy@��@�
@�K@�a@��@��@��@t�@O@�@�M@�@�d@O�@�@Ɇ@�U@�@��@A�@7@��@�}@��@�"@�x@kQ@e@�@�z@��@`B@:�@#�@�@�v@��@�4@�I@�@�u@y>@1'@�@�[@��@|�@S�@H�@C�@/�@Y@�"@�H@��@\�@GE@1�@�)@�@�h@k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��vA���A��[A��&A��HA��A���A�֡A�ޞA��ZA��A��;A���A���A���A���A��A��,A��fA��fA���A��sA��yA��KA��A��QA���A���A���A��/A��TA��A�چA��A��A��&A��.A�)_AЩ_A�~]A�{JAƹ$A��A�>�A��A��MA�l�A��A�ƨA��{A�e,A�ȴA�GA��A�+A�oA�q�A�ÖA��fA��~A�S&A�ĜA��A�~�A�cTA���A��A���A��A�'RA���A���A�cA��vA���A�xA��A�;�A�`A��PA��IA�>�A��VA��)A�-A��CA��2A���A��A��mA��A��A�~A�M�A�h�A�>A~�Ay4Av��AuK�As��Ar��Ao�oAhGEAf�Ac�AaGEA_f�A^��AY��AQ�4AN�OAN{AL��AJ�AH	AG��AF�DAE��AD�WAD}�AC�@AB��ABW�A@xA@�A?��A<˒A:ѷA9�eA8��A7�SA74A6�;A6��A6~�A5�MA4��A4&�A3��A36A2I�A/�A-�@A+MA(��A'�VA'A&��A&oiA&	A%IRA$��A$m]A$+A#�RA"�AA"(�A!�8A!�0A!r�A!A�A �A��A]dAc�A�AFAu�A��ADgA��A�7A��Ar�A�!A�zA�eA��A�|A��A�A{A��A�>AAAp�A͟A�A/A�A��AjA/�A(�A"�AeA�AaA�8A�wA��AzxA�A�A�4A�A�ADgAیAɆA|A�]Aw2A9XA�!A��A2aA�A��A��A@OA	�A
ɆA
v`A
�A	��A	H�A		A��A��A��A��AQA8�A�A�AaA*0A��A��AH�A:*A�A��A��A�An/Aa|AOvA6zA%�A�A��A�rAA�ZAƨA�~A@�A�^A�'A��AjAJA �A ��A �	A jA R�A V@���@��@�m�@���@���@�5�@���@�Z@�)�@��K@�hs@�m�@���@�c�@��@��@�M�@�hs@��@��@��F@�M@��@��@�	�@��@�*0@�4@���@��P@�@�@�ߤ@�ԕ@�o @�҉@�tT@�	@�=@�s�@�@@��M@涮@�1�@�~@�X@��@�n@���@◍@�<�@���@�(�@��+@�H@���@�P�@�֡@�J@ݜ�@���@܉�@�~@۬q@�c@�rG@�@O@�҉@ڍ�@��@ٓ@�dZ@ظR@�S�@�w2@���@��)@�Z�@���@՜�@��/@�x@Ӫ�@�t�@�c�@�1�@��@�ѷ@Ҏ�@�C�@���@�|�@���@�֡@���@�kQ@�<�@���@��Q@Ϻ^@���@�=q@�J@͓�@�K�@���@��@̇+@�3�@�
�@��@ʁo@ɭC@�1'@�l�@�Q�@��@ƌ�@�(�@�ԕ@�w2@��y@Đ.@��]@�X@³h@���@��X@���@��H@��K@�v�@��@��@���@�;d@� i@���@�*�@�hs@��@��_@�\�@��r@�A @���@�g8@�2�@��>@��V@�e,@��@���@��@�c�@�[�@�l�@�c @�Q�@�H@�B[@�Ft@��&@�N<@���@�w�@�Ov@���@�@�{J@�ں@�g8@�7@���@��	@�N<@�!-@���@��s@���@��$@�M@�G@��-@�\�@�+@��@�d�@��@���@�iD@�@@��[@�Ĝ@���@�_@���@�Vm@�0�@��@�֡@��@�i�@�!@���@�Mj@��@��9@��@���@�K�@�@@��]@���@��8@��@���@�w�@�u@���@�<6@��@�-�@���@�f�@�P�@�1�@��@��@��s@���@��h@��O@���@��L@�tT@���@��|@��1@�y>@�Q�@��@�zx@� i@���@���@��@��o@�W�@��@���@��$@�J#@�*0@�͟@���@�>B@��S@�x@�hs@�Vm@�)_@�҉@�y>@�J�@�'R@��z@�{J@�33@���@��]@�͟@��j@���@�d�@�H�@�@���@�j�@�Mj@�+@��@���@�c�@�:�@�3�@�O@��@�u�@�$t@��@�L0@��]@��@��~@�RT@�q@�V@�Ĝ@�I�@��.@��j@���@�IR@�͟@���@�tT@�W�@�1'@��Z@�rG@��@�$@���@��@�J�@���@�n�@��@�u@��@��d@�j�@�,�@��@��R@�*�@��@��@���@�c@�f�@�S�@�A @��M@�a|@��&@���@���@�)_@��@��5@��@��+@�U2@��@���@�_p@�%@��@��v@���@�R�@�G@�X@��K@���@�u%@�9X@���@�(�@��@�bN@�.�@�V@�@~�b@}�=@}|@}x�@}w2@}#�@|��@|r�@|PH@|'R@|b@{��@{��@{��@{�@{O@{�@z�X@zc @y�@y	l@w�]@w�@@wiD@w/�@v��@u��@u�)@uX@u�@t�v@t��@s�W@sP�@sY@r�c@r��@rV@r@q�)@q��@q��@oݘ@o�@n͟@n��@nE�@n@m�H@l�P@l�4@lV�@l�@l�@k�
@k��@k�:@k@O@kS@jں@jTa@i�@i��@iB�@h�@g~�@f��@f�+@e�z@dFt@c�f@b�@bi�@a�o@a�@aF@`ی@`�o@`?�@_ƨ@_��@_E9@^͟@^n�@]�d@]�h@]��@]%F@\ѷ@\_@\9X@\x@[�F@[H�@Zȴ@ZL0@Y�@Y��@YS&@Y4@Y�@Xm�@W�K@W��@Wx@V��@V��@VQ@VB[@V3�@U�@UJ�@U%F@U�@U@TɆ@T7@S�k@SJ#@R�s@RR�@Q��@Q�>@Q�@Q�9@Q�9@Q�@Q��@QIR@Q%F@P��@PV�@P4n@O�
@O�@N�m@Nv�@NV@N1�@N�@M��@MVm@MA @M�@M�@L�@Lw�@L:�@LM@L	�@K��@K�P@J�@J@IVm@I�@H�I@H!@G��@G��@Gg�@G;d@F��@E�@Em]@E#�@D�?@D~(@D@Cx@C)_@Bxl@A�@A@@@�)@@b@?� @?��@?iD@?9�@>�m@>{@=�^@=a�@<�f@<�.@<%�@;��@;W?@;"�@:�@:?@:{@9��@9�S@9}�@9X@9&�@8�j@8�@8e�@8'R@7�a@7{J@7e�@7J#@6��@6V@6Q@6M�@5��@5s�@55�@4�|@4��@4��@4�/@4�j@4��@4:�@3��@3�P@3"�@2҉@2��@2s�@2n�@2($@1�9@1��@1|@1Vm@0�@0��@0tT@0]d@0C-@/�@/g�@/�@.�@.�X@.��@.�@.�b@.��@.��@.4@-:�@-&�@-&�@-�@,��@,z�@,@+��@+t�@+�@*��@*�x@*��@*q�@*@�@*1�@)ϫ@)w2@)Y�@)Y�@)J�@)<6@)+@)V@)+@)+@(��@(��@(V�@'��@'�@'�*@'��@'��@'�@&�2@&ߤ@&�m@&u%@&@%ԕ@%�3@%�H@%�t@%�C@%�X@%�=@%=�@$��@#�A@#4�@"�@"��@"J�@"0U@!�.@!�H@!k�@!@@ ��@ Xy@��@�
@�K@�a@��@��@��@t�@O@�@�M@�@�d@O�@�@Ɇ@�U@�@��@A�@7@��@�}@��@�"@�x@kQ@e@�@�z@��@`B@:�@#�@�@�v@��@�4@�I@�@�u@y>@1'@�@�[@��@|�@S�@H�@C�@/�@Y@�"@�H@��@\�@GE@1�@�)@�@�h@k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	d�B	d�B	e,B	dZB	d�B	e`B	d�B	d�B	e`B	d�B	e�B	i�B	h�B	j�B	i�B	k�B	n�B	ncB	l�B	jB	o B	s�B	s�B	s�B	tnB	vzB	x8B	{�B	}�B	� B	�?B	��B	��B	��B	��B	��B	��B	�gB	�$B	��B
�B

rB
B
�B
jB
 BB
��B
��B<BBAB*B,BRTBcBi_B��B�B�B��B��B��B��B�2B��B��B��B��B��B�DB��B��BޞB��BѝB�/BܒB��B��B��B��B��B�CB�4Bp�Bc�BW$BF�B$@B[B
�HB
��B
�B
��B
E�B
#�B	�B	�B	��B	��B	�vB	�@B	�B	�gB	V�B	I�B	>�B	49B	*KB	#B	WB	(XB	,�B	0;B	8B	J#B	\xB	pB	��B	�+B	��B	�GB	�B	�<B	ںB	��B	��B	�[B	�@B	�5B	�4B	�B	�B	�RB	�DB	��B	�BB
-B
�B
	lB
�B
�B	��B	�B	��B	�B	�(B	�hB	��B	��B	��B	��B	�B	��B	�wB	��B	��B	��B	��B	�	B	��B	�bB	��B	�EB	�B	��B	��B	ݘB	�QB	՛B	��B	�vB	�7B	�5B	��B	��B	�DB	�UB	��B	��B	��B	�B	�PB
	7B
�B
�B
mB	��B	��B	��B	�PB	��B
 �B
�B
�B
�B
	lB
�B
jB
hB
_B
,"B
-)B
-wB
.�B
2�B
3B
3hB
3B
2�B
2�B
33B
3hB
49B
5tB
5�B
5�B
6zB
6`B
6zB
6+B
5tB
5�B
5�B
5tB
5?B
4TB
33B
2aB
1�B
2B
2B
2|B
2�B
2GB
1AB
-�B
,"B
*�B
)�B
(�B
'RB
%�B
$�B
#�B
"hB
!bB
 vB
�B
�B
�B
B
IB
�B
�B
�B
B
B
qB
�B
B
eB
�B
�B
!�B
!�B
"�B
$B
#:B
#TB
#�B
#TB
"�B
"4B
"B
!�B
"NB
"hB
"�B
"�B
"B
!HB
 �B
!HB
 �B
�B
�B
�B
B
]B
�B
5B
OB
�B
�B
�B
�B
�B
VB
6B
�B
�B
B
�B
:B
}B
.B
\B
(B
VB
�B
jB
�B
~B
�B
�B
�B
B
)B

�B

�B

#B

=B
	�B
	�B
	B
�B
�B
zB
B
YB
B
�B
B
�B
MB
B
�B
�B
-B
�B
uB
�B
;B
 �B	�HB	�HB	��B	��B	�B	�6B	��B	��B	�B	�JB	��B	�B	��B	�JB	�JB	��B	�B	��B	�wB	��B
 B
 �B
B
 �B
B
 �B
 iB
B
 �B
 OB
 iB
 OB
 iB
 4B
 �B
 iB
 4B
�B
B
 �B
 B
oB
oB
B
 �B
 B	��B	��B
;B
gB
?B
�B
�B
�B
�B
AB
B
 OB
  B
uB
B
3B
�B
B
B
�B
3B
B
�B
3B
�B
�B
�B
�B
B
3B
B
�B
�B
�B
�B
�B
mB
?B
�B
B
_B
_B
�B
zB
�B
�B
�B
B
B
�B
B
SB
�B
B
B
?B
�B
�B
_B
�B
�B
�B
KB
�B
	RB

rB
DB
�B
�B
xB
B
DB
�B
B
�B
�B
~B
�B
\B
B
B
bB
}B
}B
bB
}B
HB
bB
B
HB
bB
.B
�B
vB
NB
�B
NB
4B
�B
�B
4B
NB
4B
NB
�B
 B
:B
TB
oB
@B
[B
�B
�B
uB
@B
&B
�B
B
2B
FB
,B
aB
{B
B
�B
�B
�B
B
9B
mB
SB
�B
�B
�B
�B
�B
�B
�B
1B
�B
�B
�B
�B
�B
�B
yB
_B
�B
7B
�B
�B
�B
	B
	B
qB
�B
�B
�B
]B
xB
�B
xB
xB
�B
B
�B
�B
�B
�B
IB
B
]B
dB
dB
dB
�B
/B
dB
/B
~B
~B
�B
�B
dB
/B
�B
�B
�B
B
5B
OB
5B
B
�B
OB
OB
�B
�B
�B
 vB
 �B
 �B
 �B
!HB
!bB
!�B
!�B
#:B
#TB
#�B
"�B
#B
#B
#TB
# B
#�B
%B
%�B
%�B
&fB
'B
'RB
'8B
'�B
'�B
'�B
(sB
(�B
(�B
)yB
)�B
)yB
)�B
)�B
*0B
*eB
*eB
+B
+kB
+QB
+�B
,�B
-�B
-�B
-�B
.cB
.�B
/5B
0oB
0oB
0UB
0;B
0�B
1AB
1'B
1AB
1[B
1vB
1�B
1�B
1�B
1�B
2GB
2�B
2�B
3MB
3hB
4nB
5B
5%B
5�B
5tB
5�B
6�B
6�B
7�B
7�B
7�B
7�B
8B
8RB
8RB
8RB
8lB
8�B
8�B
8�B
8�B
88B
:^B
:�B
:�B
:�B
;dB
;B
;dB
<B
<6B
<6B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=�B
=�B
=�B
=�B
>wB
>�B
>�B
?.B
@OB
@�B
@�B
AUB
A�B
A�B
A�B
BB
A�B
BB
B[B
BAB
B[B
B�B
CB
C{B
C�B
CaB
C�B
C�B
DB
C�B
DB
DB
D�B
D�B
EB
E9B
EmB
E�B
E�B
E�B
FYB
F�B
F�B
GB
GzB
G�B
G�B
G�B
G�B
H�B
IB
H�B
H�B
H�B
IB
I�B
I�B
I�B
JXB
J�B
K)B
KB
KB
KB
J�B
J�B
KDB
K^B
K^B
K�B
LB
K�B
LB
L�B
L�B
L�B
MB
MB
M6B
MjB
M�B
M�B
M�B
M�B
M�B
N<B
NVB
NVB
N<B
NpB
NVB
O\B
OvB
P.B
PHB
P}B
P�B
P�B
Q B
QB
Q B
Q�B
Q�B
RTB
R:B
R�B
R�B
R�B
S[B
S@B
T,B
T�B
UB
T�B
VB
U�B
VB
V9B
VB
V�B
V�B
W$B
WsB
W�B
X+B
X�B
Y1B
Y1B
YKB
Y�B
Y�B
YB
YB
Y�B
ZB
Z7B
Z7B
Z�B
ZB
ZB
Y�B
Y�B
Y�B
Y�B
ZB
[	B
[�B
[�B
[�B
\)B
\xB
\]B
\CB
\CB
\CB
\�B
\�B
\�B
\�B
\�B
]/B
]dB
]�B
]�B
]~B
]�B
]�B
]�B
]�B
]�B
]�B
^jB
^�B
^�B
_B
_B
_pB
`BB
`�B
`�B
`�B
`vB
`vB
`vB
`\B
`\B
`BB
`�B
`�B
`�B
`�B
`�B
`�B
aB
a-B
aB
aHB
abB
aHB
aHB
abB
a�B
a�B
b�B
b�B
cB
c:B
cnB
cnB
dB
d�B
eB
eB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
gRB
gmB
gRB
gmB
g�B
g�B
h
B
h
B
g�B
h
B
h
B
h
B
g�B
h
B
h�B
h�B
iyB
i�B
i�B
i�B
i�B
jB
jeB
j�B
j�B
kB
k�B
k�B
k�B
k�B
k�B
lB
l"B
l"B
l"B
l=B
lqB
lqB
l�B
m)B
mCB
mwB
m�B
n/B
ncB
ncB
n}B
n}B
n�B
n�B
n�B
oOB
oOB
o�B
o�B
o�B
o�B
o�B
pB
p;B
p;B
p;B
p�B
p�B
q'B
qB
qB
p�B
q[B
rB
r-B
r|B
rGB
r�B
raB
raB
r|B
r|B
r|B
r|B
r�B
sB
shB
sMB
sMB
s�B
s�B
s�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	d�B	d�B	e,B	dZB	d�B	e`B	d�B	d�B	e`B	d�B	e�B	i�B	h�B	j�B	i�B	k�B	n�B	ncB	l�B	jB	o B	s�B	s�B	s�B	tnB	vzB	x8B	{�B	}�B	� B	�?B	��B	��B	��B	��B	��B	��B	�gB	�$B	��B
�B

rB
B
�B
jB
 BB
��B
��B<BBAB*B,BRTBcBi_B��B�B�B��B��B��B��B�2B��B��B��B��B��B�DB��B��BޞB��BѝB�/BܒB��B��B��B��B��B�CB�4Bp�Bc�BW$BF�B$@B[B
�HB
��B
�B
��B
E�B
#�B	�B	�B	��B	��B	�vB	�@B	�B	�gB	V�B	I�B	>�B	49B	*KB	#B	WB	(XB	,�B	0;B	8B	J#B	\xB	pB	��B	�+B	��B	�GB	�B	�<B	ںB	��B	��B	�[B	�@B	�5B	�4B	�B	�B	�RB	�DB	��B	�BB
-B
�B
	lB
�B
�B	��B	�B	��B	�B	�(B	�hB	��B	��B	��B	��B	�B	��B	�wB	��B	��B	��B	��B	�	B	��B	�bB	��B	�EB	�B	��B	��B	ݘB	�QB	՛B	��B	�vB	�7B	�5B	��B	��B	�DB	�UB	��B	��B	��B	�B	�PB
	7B
�B
�B
mB	��B	��B	��B	�PB	��B
 �B
�B
�B
�B
	lB
�B
jB
hB
_B
,"B
-)B
-wB
.�B
2�B
3B
3hB
3B
2�B
2�B
33B
3hB
49B
5tB
5�B
5�B
6zB
6`B
6zB
6+B
5tB
5�B
5�B
5tB
5?B
4TB
33B
2aB
1�B
2B
2B
2|B
2�B
2GB
1AB
-�B
,"B
*�B
)�B
(�B
'RB
%�B
$�B
#�B
"hB
!bB
 vB
�B
�B
�B
B
IB
�B
�B
�B
B
B
qB
�B
B
eB
�B
�B
!�B
!�B
"�B
$B
#:B
#TB
#�B
#TB
"�B
"4B
"B
!�B
"NB
"hB
"�B
"�B
"B
!HB
 �B
!HB
 �B
�B
�B
�B
B
]B
�B
5B
OB
�B
�B
�B
�B
�B
VB
6B
�B
�B
B
�B
:B
}B
.B
\B
(B
VB
�B
jB
�B
~B
�B
�B
�B
B
)B

�B

�B

#B

=B
	�B
	�B
	B
�B
�B
zB
B
YB
B
�B
B
�B
MB
B
�B
�B
-B
�B
uB
�B
;B
 �B	�HB	�HB	��B	��B	�B	�6B	��B	��B	�B	�JB	��B	�B	��B	�JB	�JB	��B	�B	��B	�wB	��B
 B
 �B
B
 �B
B
 �B
 iB
B
 �B
 OB
 iB
 OB
 iB
 4B
 �B
 iB
 4B
�B
B
 �B
 B
oB
oB
B
 �B
 B	��B	��B
;B
gB
?B
�B
�B
�B
�B
AB
B
 OB
  B
uB
B
3B
�B
B
B
�B
3B
B
�B
3B
�B
�B
�B
�B
B
3B
B
�B
�B
�B
�B
�B
mB
?B
�B
B
_B
_B
�B
zB
�B
�B
�B
B
B
�B
B
SB
�B
B
B
?B
�B
�B
_B
�B
�B
�B
KB
�B
	RB

rB
DB
�B
�B
xB
B
DB
�B
B
�B
�B
~B
�B
\B
B
B
bB
}B
}B
bB
}B
HB
bB
B
HB
bB
.B
�B
vB
NB
�B
NB
4B
�B
�B
4B
NB
4B
NB
�B
 B
:B
TB
oB
@B
[B
�B
�B
uB
@B
&B
�B
B
2B
FB
,B
aB
{B
B
�B
�B
�B
B
9B
mB
SB
�B
�B
�B
�B
�B
�B
�B
1B
�B
�B
�B
�B
�B
�B
yB
_B
�B
7B
�B
�B
�B
	B
	B
qB
�B
�B
�B
]B
xB
�B
xB
xB
�B
B
�B
�B
�B
�B
IB
B
]B
dB
dB
dB
�B
/B
dB
/B
~B
~B
�B
�B
dB
/B
�B
�B
�B
B
5B
OB
5B
B
�B
OB
OB
�B
�B
�B
 vB
 �B
 �B
 �B
!HB
!bB
!�B
!�B
#:B
#TB
#�B
"�B
#B
#B
#TB
# B
#�B
%B
%�B
%�B
&fB
'B
'RB
'8B
'�B
'�B
'�B
(sB
(�B
(�B
)yB
)�B
)yB
)�B
)�B
*0B
*eB
*eB
+B
+kB
+QB
+�B
,�B
-�B
-�B
-�B
.cB
.�B
/5B
0oB
0oB
0UB
0;B
0�B
1AB
1'B
1AB
1[B
1vB
1�B
1�B
1�B
1�B
2GB
2�B
2�B
3MB
3hB
4nB
5B
5%B
5�B
5tB
5�B
6�B
6�B
7�B
7�B
7�B
7�B
8B
8RB
8RB
8RB
8lB
8�B
8�B
8�B
8�B
88B
:^B
:�B
:�B
:�B
;dB
;B
;dB
<B
<6B
<6B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=�B
=�B
=�B
=�B
>wB
>�B
>�B
?.B
@OB
@�B
@�B
AUB
A�B
A�B
A�B
BB
A�B
BB
B[B
BAB
B[B
B�B
CB
C{B
C�B
CaB
C�B
C�B
DB
C�B
DB
DB
D�B
D�B
EB
E9B
EmB
E�B
E�B
E�B
FYB
F�B
F�B
GB
GzB
G�B
G�B
G�B
G�B
H�B
IB
H�B
H�B
H�B
IB
I�B
I�B
I�B
JXB
J�B
K)B
KB
KB
KB
J�B
J�B
KDB
K^B
K^B
K�B
LB
K�B
LB
L�B
L�B
L�B
MB
MB
M6B
MjB
M�B
M�B
M�B
M�B
M�B
N<B
NVB
NVB
N<B
NpB
NVB
O\B
OvB
P.B
PHB
P}B
P�B
P�B
Q B
QB
Q B
Q�B
Q�B
RTB
R:B
R�B
R�B
R�B
S[B
S@B
T,B
T�B
UB
T�B
VB
U�B
VB
V9B
VB
V�B
V�B
W$B
WsB
W�B
X+B
X�B
Y1B
Y1B
YKB
Y�B
Y�B
YB
YB
Y�B
ZB
Z7B
Z7B
Z�B
ZB
ZB
Y�B
Y�B
Y�B
Y�B
ZB
[	B
[�B
[�B
[�B
\)B
\xB
\]B
\CB
\CB
\CB
\�B
\�B
\�B
\�B
\�B
]/B
]dB
]�B
]�B
]~B
]�B
]�B
]�B
]�B
]�B
]�B
^jB
^�B
^�B
_B
_B
_pB
`BB
`�B
`�B
`�B
`vB
`vB
`vB
`\B
`\B
`BB
`�B
`�B
`�B
`�B
`�B
`�B
aB
a-B
aB
aHB
abB
aHB
aHB
abB
a�B
a�B
b�B
b�B
cB
c:B
cnB
cnB
dB
d�B
eB
eB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
gRB
gmB
gRB
gmB
g�B
g�B
h
B
h
B
g�B
h
B
h
B
h
B
g�B
h
B
h�B
h�B
iyB
i�B
i�B
i�B
i�B
jB
jeB
j�B
j�B
kB
k�B
k�B
k�B
k�B
k�B
lB
l"B
l"B
l"B
l=B
lqB
lqB
l�B
m)B
mCB
mwB
m�B
n/B
ncB
ncB
n}B
n}B
n�B
n�B
n�B
oOB
oOB
o�B
o�B
o�B
o�B
o�B
pB
p;B
p;B
p;B
p�B
p�B
q'B
qB
qB
p�B
q[B
rB
r-B
r|B
rGB
r�B
raB
raB
r|B
r|B
r|B
r|B
r�B
sB
shB
sMB
sMB
s�B
s�B
s�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105241  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192215  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192215  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192215                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042223  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042223  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                