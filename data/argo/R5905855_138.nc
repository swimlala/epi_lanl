CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-24T09:48:29Z creation;2022-11-24T09:48:30Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221124094829  20221124100026  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @� K�Ӡ1   @� L��@.�t�j�c����S�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A!��A@  A`  A�  A�ffA�33A�33A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�33B�  B�  B�  B�  B���B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���C   C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.33C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�3D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@xQ�@�(�@�(�A�A>{A^{A~{A�p�A�=pA�=pA�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B��\B���B�B�B�B�B��\B�B�B���B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�(�B��\B�C�HC�HC�HC�HC	�HC�HCǮC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+��C.{C/�HC1ǮC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCCǮCE�HCG�HCI�HCK�HCMǮCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]��C_��Ca�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA��DBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��\D��\D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��\D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A՚�A՘�A՗$A՘�A՛=A՜xA՜�AՖ�AՙeAաbA՜CAՖSA՘+AՒ�AՑ4A՘�AՓ�AՂuA�}"A՟�A�L0A�
rA׵�Aױ[AעhAח�AבhA׌AׄA�|A�y�A�q�A�j�A֠�A���A��A�XEA�z�AȺ*A�+kA� �A�kA�~]A� �A���A�Q�A�+A�A�A��)A��iA�($A�y>A��A�y	A�ܒA�M�A��}A��-A���A�IA��A��A�҉A���A�{A���A�w�A��iA�1A���A�&�A��xA���A�_A���A��vA���A��xA���A�>A���A���A�r�A�a�A���A���A�zA�i�A{\�AvOAt��Apl�Am�KAk�	Ah��Af|�Ad�Ab�HA_H�AY�YAW�AVqAR�MAL�AJ \AEbAA��A?��A=]�A;"hA:�A7�A5�hA4YA2��A0XyA.OA-�A+��A+$tA)�PA)�A(��A(�;A(�A(��A'��A&��A&f�A%��A%��A$��A$eA#�A#�A"�.A!�A ��Al�A �A��A��As�ATaA|�A!A�A�xA�4A"hA��AVAX�A�UA(�A��AԕA��AJ#A��A%A:�A`BA($Aw�A=qA��A��A@A��AzxA�AD�A=A��AZ�AL0Ao Ay>A�bA��A	�PA	1�A��A^5AOAm�ARTA��A_A�A�FA��Ak�A+�A8�A&A5?A�A-�A��A��A ��A f�A ��A ~�A �tA �A �AA )�A M@� i@��6@���@�Ɇ@��d@���@��m@��}@���@��@�W�@��@�v`@��]@���@��e@�[�@�1@��@�=q@�w2@@� �@�J�@�͟@�@�@�O@��@��U@�c @�,=@��@��3@濱@�Ov@傪@��@�YK@�(�@���@�V@�M@��@��@�Q�@ߞ�@���@�@�(�@��)@�$t@ڴ9@�S�@�C-@ٞ�@���@�9X@�O�@��@��@��?@�J#@�&@��/@�.�@ӟV@���@�s�@�q@ъ	@�i�@��@���@Ю}@�{J@�%F@��@��?@�2�@��@�G@ͮ@�x�@�:�@��@��@ʧ@�R�@� �@ɾw@ɮ�@ə�@�`B@�"�@��5@ȹ�@�y>@�6�@Ǵ�@ƨ�@�oi@�.�@�خ@�C�@���@ĦL@��+@å�@�a�@�(@«6@�z�@�(�@���@���@��@�V@��^@�j@�/@�,�@���@�{@��@�8�@�@��@�%@��I@�<�@���@�RT@���@�Q�@��@��4@�,=@��m@��@��@�m]@�ی@�_@��@�f�@��2@��@�ȴ@��I@�S�@���@��M@��@��	@���@�u�@�7@��@�/�@�$t@�e�@�@O@�!�@��@���@���@�~(@�1'@��@���@�:�@��R@�S�@���@�.I@��@���@�c @��@���@�!-@��E@��@�W�@��@�o�@��@���@�u�@�	�@��=@��"@���@���@�z@�7�@�@��#@���@���@��	@�RT@�&@���@�6@���@�Q�@��@��@���@�c @��D@��d@��@���@�}�@�f�@�	l@��e@��u@�Ov@�4@���@�g�@�(�@�q@��@�@@���@�ں@���@�8�@��@���@�s�@�;d@�;@���@�}V@�J�@���@��g@��@�{J@�2a@�	l@�xl@�?�@��@���@�b�@�S@���@��@���@��~@�^�@���@�d�@�	@���@���@�u�@���@��/@���@���@�~�@�U�@�%F@���@��\@�^5@��@��P@�<6@�%@���@���@�L0@��@�˒@�Vm@�(@���@���@�ff@�1'@��D@��Q@��:@�j@�\�@�@O@�o@���@��x@��@�N�@���@��V@���@���@�c�@��@��<@���@��+@�n�@�S�@�!@��A@���@�o�@�W?@�-w@��@�͟@���@�~(@�7�@�J@���@���@�Mj@�S@��,@���@�i�@� �@��#@���@�Dg@�(�@��@�@���@�Q@�M@��'@�_p@�=@��@���@�Ɇ@���@��O@��@���@�_�@|�@~�2@~��@~��@~{�@~	@}�@}�d@}�^@}\�@}�@|M@{�@z�@zL0@z{@y��@x��@xq@w�W@v��@vO@u`B@t�`@tu�@t?�@tA�@sa@r�@r��@q��@q[W@p�@pD�@o�@o�@n�@n��@n��@n�r@nYK@n �@m��@m�9@m�@lr�@l�@k�@ky�@k9�@j�2@j�R@jH�@izx@iG�@iq@h��@h�@hM@g�@g�P@gH�@g4�@g+@g�@f��@fTa@fL0@fJ�@fB[@e�3@e�~@eT�@d�P@d�@d�@d[�@c�@c4�@b�"@b�@bq�@a�@aa�@aB�@`��@_�W@_��@_�4@_o@^�,@^��@^�@^4@]j@]L�@]0�@]�@\��@\�@[iD@[,�@Z�,@Z@X��@X��@XM@X4n@X	�@W�a@W=@V�X@V:*@U�)@Uu�@U�@Toi@T  @S��@S��@S_p@S@Rں@Rc @Q�o@Q[W@Q%F@Q�@Q�@P��@P|�@PA�@O�;@N�8@NE�@M��@L�E@L��@L�@L��@Kƨ@K�F@K�@KdZ@KA�@K&@J͟@Jc @I�.@H��@H9X@G�@Gƨ@G�k@Gs@Ge�@GS�@G4�@F��@F��@F��@F��@F!�@E�@Ee,@E�@DɆ@Dy>@D_@D?�@D�@C��@B��@B��@B�@A��@A�T@A�@@��@@��@@��@?�&@?;d@?'�@? i@>ߤ@=�@<�`@<��@<:�@;��@;;d@;Y@;�@;�@:�}@:p;@:Ta@9�d@9c�@8�@8:�@8@7��@7��@7�{@7;d@7�@6�@6��@5�@5+@4�K@4�I@4!@3�@3Mj@3/�@31�@3'�@2xl@1��@1w2@1G�@0�@0��@0A�@/�Q@/�@/g�@/P�@/$t@.�y@.Ta@.R�@.H�@.6�@.�@-��@-�@,�v@,�?@,U2@+�@+�;@+��@+qv@+=@*�@*��@*Q@)�Z@)�H@)�~@)u�@)^�@)G�@)�@(֡@(��@(bN@(A�@(9X@'��@'o�@'S�@';d@&��@&�@&i�@&&�@%�@%��@%k�@%G�@$�v@$��@$u�@$M@$"h@$x@#�&@#t�@#W?@"��@"ں@"��@"R�@"�@"_@!�Z@!ϫ@!w2@!Vm@!(�@ �p@ �@ �.@ z�@ ?�@ݘ@�k@t�@W?@/�@@��@��@e@�@ԕ@�t@�=@\�@V@ی@q@`�@PH@b@��@�V@y�@a@>�@o@�8@��@��@�s@��@�A@ff@Ov@#:@�.@ԕ@�X@�@k�@?}@�5@��@��@PH@(�@�W@��@��@qv@/�@�c@�'@�x@��@^5@=q@0U@��@�d@�=@\�@0�@V@;@�P@�v@��@�z@C-@"h@M@��@��@�@g�@�@�@�@��@��@ȴ@�@��@��@\�@ �@��@ϫ@��@��@%F@�P@�@��@��@j@C-@,=@�@� @�[@�q@j�@C@�@@�]@�<@�<@��@�x@�A@\�@0U@�D@�j@�C@�h@\�@@@��@��@�@V�@7�@M@�&@��@t�@X�@@O@�@
�,@
�@
�1@
s�@
J�@
$�@
�@	��@	��@	�@	�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A՚�A՘�A՗$A՘�A՛=A՜xA՜�AՖ�AՙeAաbA՜CAՖSA՘+AՒ�AՑ4A՘�AՓ�AՂuA�}"A՟�A�L0A�
rA׵�Aױ[AעhAח�AבhA׌AׄA�|A�y�A�q�A�j�A֠�A���A��A�XEA�z�AȺ*A�+kA� �A�kA�~]A� �A���A�Q�A�+A�A�A��)A��iA�($A�y>A��A�y	A�ܒA�M�A��}A��-A���A�IA��A��A�҉A���A�{A���A�w�A��iA�1A���A�&�A��xA���A�_A���A��vA���A��xA���A�>A���A���A�r�A�a�A���A���A�zA�i�A{\�AvOAt��Apl�Am�KAk�	Ah��Af|�Ad�Ab�HA_H�AY�YAW�AVqAR�MAL�AJ \AEbAA��A?��A=]�A;"hA:�A7�A5�hA4YA2��A0XyA.OA-�A+��A+$tA)�PA)�A(��A(�;A(�A(��A'��A&��A&f�A%��A%��A$��A$eA#�A#�A"�.A!�A ��Al�A �A��A��As�ATaA|�A!A�A�xA�4A"hA��AVAX�A�UA(�A��AԕA��AJ#A��A%A:�A`BA($Aw�A=qA��A��A@A��AzxA�AD�A=A��AZ�AL0Ao Ay>A�bA��A	�PA	1�A��A^5AOAm�ARTA��A_A�A�FA��Ak�A+�A8�A&A5?A�A-�A��A��A ��A f�A ��A ~�A �tA �A �AA )�A M@� i@��6@���@�Ɇ@��d@���@��m@��}@���@��@�W�@��@�v`@��]@���@��e@�[�@�1@��@�=q@�w2@@� �@�J�@�͟@�@�@�O@��@��U@�c @�,=@��@��3@濱@�Ov@傪@��@�YK@�(�@���@�V@�M@��@��@�Q�@ߞ�@���@�@�(�@��)@�$t@ڴ9@�S�@�C-@ٞ�@���@�9X@�O�@��@��@��?@�J#@�&@��/@�.�@ӟV@���@�s�@�q@ъ	@�i�@��@���@Ю}@�{J@�%F@��@��?@�2�@��@�G@ͮ@�x�@�:�@��@��@ʧ@�R�@� �@ɾw@ɮ�@ə�@�`B@�"�@��5@ȹ�@�y>@�6�@Ǵ�@ƨ�@�oi@�.�@�خ@�C�@���@ĦL@��+@å�@�a�@�(@«6@�z�@�(�@���@���@��@�V@��^@�j@�/@�,�@���@�{@��@�8�@�@��@�%@��I@�<�@���@�RT@���@�Q�@��@��4@�,=@��m@��@��@�m]@�ی@�_@��@�f�@��2@��@�ȴ@��I@�S�@���@��M@��@��	@���@�u�@�7@��@�/�@�$t@�e�@�@O@�!�@��@���@���@�~(@�1'@��@���@�:�@��R@�S�@���@�.I@��@���@�c @��@���@�!-@��E@��@�W�@��@�o�@��@���@�u�@�	�@��=@��"@���@���@�z@�7�@�@��#@���@���@��	@�RT@�&@���@�6@���@�Q�@��@��@���@�c @��D@��d@��@���@�}�@�f�@�	l@��e@��u@�Ov@�4@���@�g�@�(�@�q@��@�@@���@�ں@���@�8�@��@���@�s�@�;d@�;@���@�}V@�J�@���@��g@��@�{J@�2a@�	l@�xl@�?�@��@���@�b�@�S@���@��@���@��~@�^�@���@�d�@�	@���@���@�u�@���@��/@���@���@�~�@�U�@�%F@���@��\@�^5@��@��P@�<6@�%@���@���@�L0@��@�˒@�Vm@�(@���@���@�ff@�1'@��D@��Q@��:@�j@�\�@�@O@�o@���@��x@��@�N�@���@��V@���@���@�c�@��@��<@���@��+@�n�@�S�@�!@��A@���@�o�@�W?@�-w@��@�͟@���@�~(@�7�@�J@���@���@�Mj@�S@��,@���@�i�@� �@��#@���@�Dg@�(�@��@�@���@�Q@�M@��'@�_p@�=@��@���@�Ɇ@���@��O@��@���@�_�@|�@~�2@~��@~��@~{�@~	@}�@}�d@}�^@}\�@}�@|M@{�@z�@zL0@z{@y��@x��@xq@w�W@v��@vO@u`B@t�`@tu�@t?�@tA�@sa@r�@r��@q��@q[W@p�@pD�@o�@o�@n�@n��@n��@n�r@nYK@n �@m��@m�9@m�@lr�@l�@k�@ky�@k9�@j�2@j�R@jH�@izx@iG�@iq@h��@h�@hM@g�@g�P@gH�@g4�@g+@g�@f��@fTa@fL0@fJ�@fB[@e�3@e�~@eT�@d�P@d�@d�@d[�@c�@c4�@b�"@b�@bq�@a�@aa�@aB�@`��@_�W@_��@_�4@_o@^�,@^��@^�@^4@]j@]L�@]0�@]�@\��@\�@[iD@[,�@Z�,@Z@X��@X��@XM@X4n@X	�@W�a@W=@V�X@V:*@U�)@Uu�@U�@Toi@T  @S��@S��@S_p@S@Rں@Rc @Q�o@Q[W@Q%F@Q�@Q�@P��@P|�@PA�@O�;@N�8@NE�@M��@L�E@L��@L�@L��@Kƨ@K�F@K�@KdZ@KA�@K&@J͟@Jc @I�.@H��@H9X@G�@Gƨ@G�k@Gs@Ge�@GS�@G4�@F��@F��@F��@F��@F!�@E�@Ee,@E�@DɆ@Dy>@D_@D?�@D�@C��@B��@B��@B�@A��@A�T@A�@@��@@��@@��@?�&@?;d@?'�@? i@>ߤ@=�@<�`@<��@<:�@;��@;;d@;Y@;�@;�@:�}@:p;@:Ta@9�d@9c�@8�@8:�@8@7��@7��@7�{@7;d@7�@6�@6��@5�@5+@4�K@4�I@4!@3�@3Mj@3/�@31�@3'�@2xl@1��@1w2@1G�@0�@0��@0A�@/�Q@/�@/g�@/P�@/$t@.�y@.Ta@.R�@.H�@.6�@.�@-��@-�@,�v@,�?@,U2@+�@+�;@+��@+qv@+=@*�@*��@*Q@)�Z@)�H@)�~@)u�@)^�@)G�@)�@(֡@(��@(bN@(A�@(9X@'��@'o�@'S�@';d@&��@&�@&i�@&&�@%�@%��@%k�@%G�@$�v@$��@$u�@$M@$"h@$x@#�&@#t�@#W?@"��@"ں@"��@"R�@"�@"_@!�Z@!ϫ@!w2@!Vm@!(�@ �p@ �@ �.@ z�@ ?�@ݘ@�k@t�@W?@/�@@��@��@e@�@ԕ@�t@�=@\�@V@ی@q@`�@PH@b@��@�V@y�@a@>�@o@�8@��@��@�s@��@�A@ff@Ov@#:@�.@ԕ@�X@�@k�@?}@�5@��@��@PH@(�@�W@��@��@qv@/�@�c@�'@�x@��@^5@=q@0U@��@�d@�=@\�@0�@V@;@�P@�v@��@�z@C-@"h@M@��@��@�@g�@�@�@�@��@��@ȴ@�@��@��@\�@ �@��@ϫ@��@��@%F@�P@�@��@��@j@C-@,=@�@� @�[@�q@j�@C@�@@�]@�<@�<@��@�x@�A@\�@0U@�D@�j@�C@�h@\�@@@��@��@�@V�@7�@M@�&@��@t�@X�@@O@�@
�,@
�@
�1@
s�@
J�@
$�@
�@	��@	��@	�@	�@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	-�B	-�B	-�B	-�B	-�B	-�B	-�B	-�B	.B	-�B	.IB	.�B	.�B	/�B	0;B	/5B	0�B	6B	9�B	N�B	��B
O�B
�B
�)B
�	B
ڠB
��B
�=B
�=B
�#B
ۦB
�]B
ܬB
�B
�DB
�IB �BBYB>�BR BVBE�Bo�ButBt�B��B��B�?B��B�~B��B��B}�Bu�Bm)Bk�B�mBp�Bi_BL�BB�BՁB��B��B��B�BtBcBTFB2aB,B�B
�B
�}B
߾B
�@B
�1B
��B
��B
��B
��B
�MB
s�B
I�B
>]B
.}B
�B	��B	� B	�xB	��B	�7B	�0B	}B	n�B	d�B	TaB	8lB	*KB	#�B	�B��B�B��B�B��B�cB�B��B�BǔB�RB��B� B� B�B�aB��B�sB�WB�B�B��B��B�B�B�B��B�B��B�dB	YB	
=B	�B	)B	YB	;B	�B	.B	pB	!�B	5%B	>BB	?.B	>�B	?HB	BAB	F�B	M�B	R�B	Z�B	_!B	cTB	e�B	g�B	gB	f�B	iDB	tB	u�B	v�B	wfB	utB	u%B	s�B	s�B	wfB	�AB	�MB	��B	�~B	��B	��B	�ZB	��B	�eB	�zB	��B	��B	��B	��B	��B	��B	�2B	�)B	�=B	�QB	�cB	��B	��B	�B	�B	�xB	��B	��B	�B	��B	ǔB	�OB	�xB	��B	��B	��B	ȚB	�B	ҽB	�B	��B	�4B	�(B	˒B	��B	�pB	οB	�JB	�~B	̳B	�jB	��B	�NB	�FB	�FB	өB	�:B	ΥB	�B	�B	�B	�B	�B	�GB	��B	ƨB	ƨB	�XB	͹B	̈́B	˒B	�B	ɆB	˒B	��B	͟B	˒B	��B	�B	żB	żB	��B	�YB	ňB	�3B	��B	�MB	�B	ŢB	�SB	ĶB	�B	�gB	ðB	�YB	ɠB	�\B	��B	�uB	��B	��B	�B	յB	��B	߾B	�B	�B	�B	��B	�&B	�B	�@B	�B	�AB	��B	��B	�-B	�;B	�B	�iB	�B	�/B	�B	�5B	��B	��B	�"B	�)B	�B	�B	�B	�sB	��B	��B	��B	�KB	�eB	�B	�B	�qB	�B	��B	��B	�qB	��B	�B	�=B	�B	�B	�B	��B	��B	�qB	��B	�OB	�B	�IB	��B	��B	��B	�IB	��B	��B	� B	��B	�B	�AB	�B	�B	�B	�9B	�`B	��B	�LB	�>B	��B	�B	�zB	�B	�B	�`B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�rB	�zB	��B	��B	��B	�fB	��B	�lB	�*B	��B
 �B
�B
�B
%B
B
�B
SB
�B
B
KB
	RB
�B
�B
�B
�B
�B
�B
fB
	lB

XB

XB

XB
B

�B

	B
	�B
	�B
	�B
	�B
	�B

	B

=B

rB

rB

�B

�B
)B
^B
�B
B
�B
B
6B
dB
xB
xB
�B
�B
�B
�B
B
�B
B
PB
B
�B
�B
6B
B
PB
�B
"B
VB
(B
(B
(B
B
(B
\B
bB
�B
�B
�B
4B
hB
B
�B
[B
B
B
MB
�B
�B
9B
B
$B
�B
_B
�B
�B
�B
1B
�B
�B
�B
7B
qB
�B
)B
CB
�B
�B
�B
dB
�B
�B
!B
;B
VB
�B
 \B
 �B
!B
!|B
!�B
"B
"�B
"�B
# B
$&B
$�B
%�B
&2B
&LB
&�B
&�B
'B
'B
'B
'RB
'RB
'mB
'�B
(>B
(�B
)*B
)DB
)�B
*B
*�B
*�B
*�B
+B
+QB
+�B
+�B
+�B
+�B
+�B
,WB
,�B
,�B
-CB
-)B
-]B
-�B
-�B
.B
.}B
/ B
/B
/ B
/iB
0!B
0�B
0�B
1B
1AB
1�B
1�B
2-B
2|B
2�B
2�B
2|B
33B
3�B
3�B
4�B
4�B
4�B
5B
5B
5?B
5%B
5?B
5%B
4�B
5?B
5tB
5�B
5�B
5�B
5�B
6FB
6+B
6+B
6+B
5�B
6+B
6�B
7fB
7�B
8B
8B
8�B
8lB
8lB
8lB
8B
7�B
7�B
7�B
88B
8�B
:*B
9�B
:DB
;dB
:�B
:xB
:DB
:DB
:DB
:DB
:�B
;dB
<B
<�B
=<B
=�B
>�B
?�B
@iB
@B
@iB
@OB
@B
@4B
@�B
@iB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A;B
AoB
AUB
A�B
A�B
A�B
A�B
B'B
B'B
B'B
BAB
B�B
B�B
B�B
B�B
B�B
C-B
CGB
C�B
DgB
D3B
D�B
D�B
EB
E�B
E�B
F�B
G+B
G�B
G�B
HKB
H�B
H�B
H�B
IlB
J	B
J=B
JrB
JrB
JXB
J�B
J�B
J�B
JXB
J	B
I�B
I�B
I�B
I�B
J=B
J�B
J�B
J�B
J�B
J�B
KB
KDB
K^B
K�B
LJB
L0B
L�B
MB
M6B
M�B
M�B
M�B
NB
N<B
O(B
P.B
P�B
P�B
P�B
P�B
P�B
P.B
P.B
P.B
QhB
Q�B
Q4B
Q B
P�B
P�B
QB
QNB
R�B
SuB
S�B
S�B
R�B
SB
S�B
SuB
S[B
S@B
TaB
T�B
VB
V�B
WsB
XEB
W�B
XyB
X�B
YB
YB
YB
Y�B
Y�B
Y�B
Y�B
Z�B
[�B
[�B
[�B
[�B
\)B
\B
[�B
[�B
\�B
]~B
]�B
]�B
]�B
^5B
]�B
]�B
]�B
]�B
^�B
_B
^�B
_!B
`B
`BB
`\B
`�B
aB
a�B
bNB
bhB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
cB
c�B
d�B
d�B
e,B
e�B
g8B
g�B
h$B
hsB
h�B
i*B
iDB
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
jeB
j�B
j�B
kB
k6B
k�B
k�B
k�B
k�B
k�B
lB
lB
lWB
l�B
l�B
mB
mB
m)B
mCB
mwB
m�B
nB
nB
n/B
nB
n�B
n�B
n�B
n�B
oB
oiB
o�B
o�B
o�B
o�B
p!B
pB
p�B
p�B
p�B
qB
q'B
q'B
q'B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
s3B
s3B
sMB
s�B
s�B
s�B
s�B
tB
t�B
t�B
u?B
utB
u�B
u�B
vB
v�B
v�B
v�B
wB
w2B
w2B
w�B
w�B
xB
x�B
x�B
x�B
x�B
y$B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
{B
{JB
{B
{B
{�B
{�B
|B
|B
|B
|jB
|�B
|�B
|�B
}"B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
HB
cB
cB
�B
�B
�B
�B
�OB
�iB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
�uB
��B
��B
�B
�B
�GB
�aB
�aB
�aB
��B
��B
��B
��B
�3B
�3B
�3B
�MB
�gB
�gB
�MB
��B
��B
��B
��B
�9B
�SB
��B
��B
��B
�%B
�?B
�YB
��B
��B
��B
�B
�B
�_B
��B
��B
��B
�1B
��B
��B
��B
��B
�B
�RB
��B
�lB
��B
��B
�#B
�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	-�B	-�B	-�B	-�B	-�B	-�B	-�B	-�B	.B	-�B	.IB	.�B	.�B	/�B	0;B	/5B	0�B	6B	9�B	N�B	��B
O�B
�B
�)B
�	B
ڠB
��B
�=B
�=B
�#B
ۦB
�]B
ܬB
�B
�DB
�IB �BBYB>�BR BVBE�Bo�ButBt�B��B��B�?B��B�~B��B��B}�Bu�Bm)Bk�B�mBp�Bi_BL�BB�BՁB��B��B��B�BtBcBTFB2aB,B�B
�B
�}B
߾B
�@B
�1B
��B
��B
��B
��B
�MB
s�B
I�B
>]B
.}B
�B	��B	� B	�xB	��B	�7B	�0B	}B	n�B	d�B	TaB	8lB	*KB	#�B	�B��B�B��B�B��B�cB�B��B�BǔB�RB��B� B� B�B�aB��B�sB�WB�B�B��B��B�B�B�B��B�B��B�dB	YB	
=B	�B	)B	YB	;B	�B	.B	pB	!�B	5%B	>BB	?.B	>�B	?HB	BAB	F�B	M�B	R�B	Z�B	_!B	cTB	e�B	g�B	gB	f�B	iDB	tB	u�B	v�B	wfB	utB	u%B	s�B	s�B	wfB	�AB	�MB	��B	�~B	��B	��B	�ZB	��B	�eB	�zB	��B	��B	��B	��B	��B	��B	�2B	�)B	�=B	�QB	�cB	��B	��B	�B	�B	�xB	��B	��B	�B	��B	ǔB	�OB	�xB	��B	��B	��B	ȚB	�B	ҽB	�B	��B	�4B	�(B	˒B	��B	�pB	οB	�JB	�~B	̳B	�jB	��B	�NB	�FB	�FB	өB	�:B	ΥB	�B	�B	�B	�B	�B	�GB	��B	ƨB	ƨB	�XB	͹B	̈́B	˒B	�B	ɆB	˒B	��B	͟B	˒B	��B	�B	żB	żB	��B	�YB	ňB	�3B	��B	�MB	�B	ŢB	�SB	ĶB	�B	�gB	ðB	�YB	ɠB	�\B	��B	�uB	��B	��B	�B	յB	��B	߾B	�B	�B	�B	��B	�&B	�B	�@B	�B	�AB	��B	��B	�-B	�;B	�B	�iB	�B	�/B	�B	�5B	��B	��B	�"B	�)B	�B	�B	�B	�sB	��B	��B	��B	�KB	�eB	�B	�B	�qB	�B	��B	��B	�qB	��B	�B	�=B	�B	�B	�B	��B	��B	�qB	��B	�OB	�B	�IB	��B	��B	��B	�IB	��B	��B	� B	��B	�B	�AB	�B	�B	�B	�9B	�`B	��B	�LB	�>B	��B	�B	�zB	�B	�B	�`B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�rB	�zB	��B	��B	��B	�fB	��B	�lB	�*B	��B
 �B
�B
�B
%B
B
�B
SB
�B
B
KB
	RB
�B
�B
�B
�B
�B
�B
fB
	lB

XB

XB

XB
B

�B

	B
	�B
	�B
	�B
	�B
	�B

	B

=B

rB

rB

�B

�B
)B
^B
�B
B
�B
B
6B
dB
xB
xB
�B
�B
�B
�B
B
�B
B
PB
B
�B
�B
6B
B
PB
�B
"B
VB
(B
(B
(B
B
(B
\B
bB
�B
�B
�B
4B
hB
B
�B
[B
B
B
MB
�B
�B
9B
B
$B
�B
_B
�B
�B
�B
1B
�B
�B
�B
7B
qB
�B
)B
CB
�B
�B
�B
dB
�B
�B
!B
;B
VB
�B
 \B
 �B
!B
!|B
!�B
"B
"�B
"�B
# B
$&B
$�B
%�B
&2B
&LB
&�B
&�B
'B
'B
'B
'RB
'RB
'mB
'�B
(>B
(�B
)*B
)DB
)�B
*B
*�B
*�B
*�B
+B
+QB
+�B
+�B
+�B
+�B
+�B
,WB
,�B
,�B
-CB
-)B
-]B
-�B
-�B
.B
.}B
/ B
/B
/ B
/iB
0!B
0�B
0�B
1B
1AB
1�B
1�B
2-B
2|B
2�B
2�B
2|B
33B
3�B
3�B
4�B
4�B
4�B
5B
5B
5?B
5%B
5?B
5%B
4�B
5?B
5tB
5�B
5�B
5�B
5�B
6FB
6+B
6+B
6+B
5�B
6+B
6�B
7fB
7�B
8B
8B
8�B
8lB
8lB
8lB
8B
7�B
7�B
7�B
88B
8�B
:*B
9�B
:DB
;dB
:�B
:xB
:DB
:DB
:DB
:DB
:�B
;dB
<B
<�B
=<B
=�B
>�B
?�B
@iB
@B
@iB
@OB
@B
@4B
@�B
@iB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A;B
AoB
AUB
A�B
A�B
A�B
A�B
B'B
B'B
B'B
BAB
B�B
B�B
B�B
B�B
B�B
C-B
CGB
C�B
DgB
D3B
D�B
D�B
EB
E�B
E�B
F�B
G+B
G�B
G�B
HKB
H�B
H�B
H�B
IlB
J	B
J=B
JrB
JrB
JXB
J�B
J�B
J�B
JXB
J	B
I�B
I�B
I�B
I�B
J=B
J�B
J�B
J�B
J�B
J�B
KB
KDB
K^B
K�B
LJB
L0B
L�B
MB
M6B
M�B
M�B
M�B
NB
N<B
O(B
P.B
P�B
P�B
P�B
P�B
P�B
P.B
P.B
P.B
QhB
Q�B
Q4B
Q B
P�B
P�B
QB
QNB
R�B
SuB
S�B
S�B
R�B
SB
S�B
SuB
S[B
S@B
TaB
T�B
VB
V�B
WsB
XEB
W�B
XyB
X�B
YB
YB
YB
Y�B
Y�B
Y�B
Y�B
Z�B
[�B
[�B
[�B
[�B
\)B
\B
[�B
[�B
\�B
]~B
]�B
]�B
]�B
^5B
]�B
]�B
]�B
]�B
^�B
_B
^�B
_!B
`B
`BB
`\B
`�B
aB
a�B
bNB
bhB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
cB
c�B
d�B
d�B
e,B
e�B
g8B
g�B
h$B
hsB
h�B
i*B
iDB
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
jeB
j�B
j�B
kB
k6B
k�B
k�B
k�B
k�B
k�B
lB
lB
lWB
l�B
l�B
mB
mB
m)B
mCB
mwB
m�B
nB
nB
n/B
nB
n�B
n�B
n�B
n�B
oB
oiB
o�B
o�B
o�B
o�B
p!B
pB
p�B
p�B
p�B
qB
q'B
q'B
q'B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
s3B
s3B
sMB
s�B
s�B
s�B
s�B
tB
t�B
t�B
u?B
utB
u�B
u�B
vB
v�B
v�B
v�B
wB
w2B
w2B
w�B
w�B
xB
x�B
x�B
x�B
x�B
y$B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
{B
{JB
{B
{B
{�B
{�B
|B
|B
|B
|jB
|�B
|�B
|�B
}"B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
HB
cB
cB
�B
�B
�B
�B
�OB
�iB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
�uB
��B
��B
�B
�B
�GB
�aB
�aB
�aB
��B
��B
��B
��B
�3B
�3B
�3B
�MB
�gB
�gB
�MB
��B
��B
��B
��B
�9B
�SB
��B
��B
��B
�%B
�?B
�YB
��B
��B
��B
�B
�B
�_B
��B
��B
��B
�1B
��B
��B
��B
��B
�B
�RB
��B
�lB
��B
��B
�#B
�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221124094727  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221124094829  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221124094830  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221124094830                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221124094830  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221124094830  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221124100026                      G�O�G�O�G�O�                