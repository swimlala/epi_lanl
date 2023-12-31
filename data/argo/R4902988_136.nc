CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-01T03:51:29Z creation;2023-07-01T03:51:30Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230701035129  20230701043044  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�7�eC!1   @�7Ǯ{@;��+�c������1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�33A�  A���A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B��B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B���B���B�  B�  C   C  C  C  C  C	�fC  C�C  C�fC�fC  C  C  C�C�C   C"  C$  C&�C(  C*  C,�C.  C/�fC2  C4  C5�fC8  C:  C;�fC>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"y�D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'�fD(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DVy�DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_y�D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDsfDs� DtfDt�fDufDu� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}� D~  D~� D~��D� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��3D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D�  D�@ D�� D���D�  D�C3D��3D��3D�3D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D��3D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�3D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D���D�@ DՀ D�� D�  D�<�Dր D�� D�  D�@ D׀ D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڃ3D��3D�  D�<�Dۀ D�� D���D�@ D܀ D�� D�  D�@ D݃3D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�3D�C3D� D��3D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�C3D� D�� D�  D�@ D� D��3D�  D�<�D� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�(�@�(�A{A>{A^{A~{A�
=A�=pA�
=A��
A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B���B�B�B�B�B�B�B��\B�B�B�B���B�B�B�B�B�B�Bӏ\B�B�B�B���B�B�B�\B�\B�B�B�C�HC�HC�HC�HC	ǮC�HC��C�HCǮCǮC�HC�HC�HC��C��C�HC!�HC#�HC%��C'�HC)�HC+��C-�HC/ǮC1�HC3�HC5ǮC7�HC9�HC;ǮC=�HC?ǮCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY��C[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw��Cy�HC{�HC}�HCǮC��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C���C��C��qC���C���C��C��C��C��C��C���C���C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��qC��qC��C��C���C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C���C��C��qC��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��qC��C��C��C��C��qC��C��C��C��C��C��qC��C��C��qC��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDq�D�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD~�D��DxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD��DxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD~�D�RDxRD�RD xRD �RD!xRD!�RD"q�D"�RD#~�D#�RD$xRD$�RD%xRD%�RD&xRD&�RD'~�D'��D(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-q�D-�RD.xRD.�RD/q�D/�RD0xRD0�RD1xRD1�RD2xRD2��D3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;q�D;�RD<xRD<�RD=xRD=�RD>q�D>�RD?xRD?�RD@xRD@�RDAq�DA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVq�DV�RDW~�DW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\q�D\�RD]xRD]�RD^xRD^�RD_q�D_�RD`xRD`��DaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDeq�De�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDr~�Dr��DsxRDs��Dt~�Dt��DuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|��D}xRD}�RD~xRD~��DxRD��D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�x�D���D��)D�<)D�|)D���D���D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�x�D��)D��\D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D���D��)D�<)D�\D��)D��)D�<)D�x�D��)D��)D�<)D�|)D��)D��)D�<)D�|)D���D��)D�<)D�|)D���D��)D�<)D�|)D���D��)D�?\D�\D��\D��\D�<)D�|)D��)D��)D�<)D�|)D���D��)D�<)D�\D��)D��)D�<)D�|)D��)D��)D�8�D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D���D��)D�?\D�\D��)D��)D�8�D�x�D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D�\D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�8�D�x�D��)D��)D�<)D�|)D��)D��)D�8�D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�x�Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��\D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D���D�<)D�|)Dռ)D��)D�8�D�|)Dּ)D��)D�<)D�|)D׼)D��\D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�?\D�\Dڿ\D��)D�8�D�|)Dۼ)D���D�<)D�|)Dܼ)D��)D�<)D�\Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��\D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�\D��\D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�\D��)D��\D�?\D�|)D�\D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�8�D�|)D�)D��)D�?\D�|)D�)D��)D�<)D�|)D�\D��)D�8�D�|)D��\D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�/\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A˛�A�h�A�2�A� �A��A�A�
�A��A���A�[#A���AɌ�A�_pA��A�YA���A��3A�[#A�A�+A�e�A��A���A��A���A�YKA���A�[�A�VA�f�A���A��rA���A��A���A��%A�GA���A��xA�J�A���A�oA�	�A���A�.A��NA�3hA�4�A�e,A�/�A�A�MA��`A�U2A� iA��A���A��A�bNA�\�A�1A��A��9A�_;A���A��TA��0A�2�A�>�A��BA��FA�.}A�j�A�VA��A��GA��A�W�A��WA��AA��:A��gA�CaA�ʌA�y�A���A�S&A��A�#A�sA��A� �A�)_A�D3A���A�f�A�[�A}�8A}�A|�rA{�$Az��Ay�AyH�Ax��Aw��Au�VAt�$Ar�XAo��AnJAm:�Ak��Aj:*Ai�Ag�CAf|�Ae8�Ach�Aan�A_�]A^��A]�#A]�A\|�A[n/AY�AV��AT�KAR��AQ��APqvAO'RAN�HANtTAM�AK�oAJ~(AI�AHxlAH �AG��AF�AFF�AE�HAD�yAC�dAB�UAA��A@��A?�cA>��A>�A=��A=
�A<��A:�	A9z�A8�A7jA6�&A6Z�A6�A5U�A4`BA3-A2e�A2�A1��A1L0A0�rA0 �A/�A.=�A-��A-CA,��A,A+�mA+8A*,=A)MA(��A(\)A()�A(�A'��A&y>A%zxA%�A$��A$d�A#��A"~A!1�A �A��A�4A�A �A��A��AR�A��A�_A��A�AOvA͟AL�A�HAjA�A%FAQ�AJ#A�gAcAS�A�;A��A=qAffA�qA�A \A�AS�AT�A
�tA
�A	�{A	DgA�A;�AA?}A�xA?�A�MAo A�A��A<�AjA��A$�A \A ��@�+�@���@��@@��@���@��|@��
@���@�v`@�A @��@�i�@���@�M@�S&@�o@�~@�q@�!�@�K�@��@�@���@��@�X�@��}@�� @�Z�@��g@�/@ܦL@�e@ۥ@لM@־@ԦL@��@�zx@�w�@̏\@���@��@Ʌ@ȦL@�b@�%�@İ!@��@°!@���@��M@�GE@�iD@�Ft@��P@�~�@��~@��@�)_@���@���@�($@��Z@��V@�$t@�҉@�z@�֡@���@��@���@��L@���@�.I@��m@��'@�~(@�u@�ϫ@���@�L0@�)_@��.@�[W@���@���@�c�@�'R@�/@��@�i�@�f�@��@��@��@���@��B@�v�@�,=@�w2@��@��R@�h
@�3�@�U�@���@�&�@���@�z�@��@��3@�s@���@���@��L@�PH@���@���@�i�@�g8@�ff@�]d@��+@��@@�O@�v�@�PH@�1�@��;@���@�X@�P�@��f@�}V@��I@�\)@��]@��.@��P@�H�@�^�@�O�@�zx@�C-@��@�l�@���@�@���@���@�0�@�4n@�@O@��@��,@��@��v@��/@���@���@�2�@�;�@�;�@���@�b�@���@�]d@�<�@��@��0@�_p@��`@���@��@�@@�,=@��@��N@�$t@�Ɇ@���@�?�@���@���@��~@�Vm@���@��b@���@���@�hs@�U�@�e�@�e�@�a@�#�@�#�@�o�@�.I@���@��@��q@���@� �@��@�ԕ@���@�.I@��"@��[@��9@���@�@~��@}S&@}V@}@@|�O@|9X@{�V@z�@z�B@z�@z��@zxl@zs�@zQ@z
�@y�@yx�@y+�@x�v@x�e@xj@w��@wRT@v�@vd�@v�@u��@u��@u��@u�@t]d@tM@tZ@s�
@s8@r&�@q��@qVm@p[�@o��@ov`@oX�@oC�@o8@o,�@oC@nߤ@n)�@mc@m@l��@l7@k��@k��@k4�@j��@i�-@ie,@i2a@h��@h*�@g�@g�
@g��@g�@gK�@f�R@fs�@f\�@fC�@f)�@f�@f�@e��@e��@d��@dz�@d	�@c�$@cC@c�@c�@b�M@b}V@b!�@a \@`�p@`/�@_x@_!-@^��@^��@^-@]ϫ@]��@]a�@]X@]X@]Vm@]X@]L�@]A @]=�@]:�@]5�@\�E@\|�@\<�@\@[��@Z��@ZC�@Y��@YX@Xѷ@XU2@X�@W�$@Wn/@WS�@V�<@U�@U��@U�H@U�=@Uf�@U2a@U!�@T��@Tѷ@Tj@T,=@S��@S�4@S=@R��@R:*@Q�H@Q�@Q��@Q}�@Q&�@P��@P��@P��@Pw�@PXy@P/�@P*�@P%�@P�@Oƨ@Oo�@O33@N~�@M�D@M��@M�d@M��@M��@M�h@M��@M^�@M�@L��@L|�@LI�@L�@K��@KZ�@J��@Ju@I%F@H�9@HK^@H$@G�a@G]�@G!-@F��@F�X@F�F@F��@F\�@F?@F6�@F3�@F!�@F �@E�@E�z@E7L@D��@D]d@DM@D7@C�@C��@C1�@CY@B��@B�@A�N@A�@A-w@@�I@@*�@@�@?�@?t�@>��@>��@>n�@>Z�@>M�@=�T@=�t@=u�@<��@<��@<�4@<j@<M@;|�@;�@;�@:�@:�H@:�m@:�x@:��@:^5@:0U@:	@9��@9�-@9S&@98�@8�@8w�@8<�@7�@7��@7�@6�M@6�2@6�'@6}V@6.�@6�@6@5�'@54@4�@4��@4��@4]d@3��@3خ@3��@3/�@2�M@2�+@2{�@2l�@2h
@2Ta@2O@2
�@2u@1��@1�@1��@1��@1?}@1�@1�@0�@0�@0�o@0r�@0M@/�;@/��@/{J@/l�@/n/@/RT@/�@.�2@.\�@-}�@,�K@,ی@,ѷ@,�E@,�@,K^@+��@+˒@+��@+�@*�!@*s�@*Ov@*1�@*�@)�@)�-@)u�@)+�@(Ɇ@(�@(S�@(~@'خ@'�[@'��@'=@&��@&��@&z@&�@&�@%�j@%��@%c�@%5�@%%@$��@$|�@$I�@$/�@#�@#��@#�4@#O@#C@"�y@"�H@"�X@"�\@"\�@"�@!�Z@!�C@![W@!;@ Ɇ@ �@ 4n@��@C�@�@�@҉@�m@��@�x@�r@��@�@��@^5@B[@($@��@�S@hs@A @��@��@�5@�5@�`@��@��@��@e�@A�@7@�]@ݘ@�4@J#@�@�@�@͟@��@�A@p;@a|@W�@M�@J�@H�@B[@$�@�@��@u�@X@/@�@�@l"@x@qv@�@��@a|@;�@#:@�@�T@�@�@�#@�@�@�#@��@�9@�d@�C@|@s�@hs@O�@!�@�@�)@�_@>B@�m@�$@qv@E9@33@
=@��@� @^5@0U@-@�@��@��@��@�@7L@�@��@�@�I@q@A�@��@��@��@�[@��@�@@|�@F�@�@�@�@ں@��@��@�x@}V@M�@)�@�@�T@ϫ@�@m]@[W@0�@��@�?@��@�@j@2�@�
@��@C�@
�@
�X@
�@
��@
h
@
)�@
	@	�.@	ϫ@	�C@	�h@	0�@�)@��@]d@/�@x@�@�0@��@_p@>�@�@�@�,@�!@{�@GE@6�@&�@�@�.@��@�@��@�@w2@=�@@��@��@~(@z�@r�@A�@�]@��@�a@�[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A˛�A�h�A�2�A� �A��A�A�
�A��A���A�[#A���AɌ�A�_pA��A�YA���A��3A�[#A�A�+A�e�A��A���A��A���A�YKA���A�[�A�VA�f�A���A��rA���A��A���A��%A�GA���A��xA�J�A���A�oA�	�A���A�.A��NA�3hA�4�A�e,A�/�A�A�MA��`A�U2A� iA��A���A��A�bNA�\�A�1A��A��9A�_;A���A��TA��0A�2�A�>�A��BA��FA�.}A�j�A�VA��A��GA��A�W�A��WA��AA��:A��gA�CaA�ʌA�y�A���A�S&A��A�#A�sA��A� �A�)_A�D3A���A�f�A�[�A}�8A}�A|�rA{�$Az��Ay�AyH�Ax��Aw��Au�VAt�$Ar�XAo��AnJAm:�Ak��Aj:*Ai�Ag�CAf|�Ae8�Ach�Aan�A_�]A^��A]�#A]�A\|�A[n/AY�AV��AT�KAR��AQ��APqvAO'RAN�HANtTAM�AK�oAJ~(AI�AHxlAH �AG��AF�AFF�AE�HAD�yAC�dAB�UAA��A@��A?�cA>��A>�A=��A=
�A<��A:�	A9z�A8�A7jA6�&A6Z�A6�A5U�A4`BA3-A2e�A2�A1��A1L0A0�rA0 �A/�A.=�A-��A-CA,��A,A+�mA+8A*,=A)MA(��A(\)A()�A(�A'��A&y>A%zxA%�A$��A$d�A#��A"~A!1�A �A��A�4A�A �A��A��AR�A��A�_A��A�AOvA͟AL�A�HAjA�A%FAQ�AJ#A�gAcAS�A�;A��A=qAffA�qA�A \A�AS�AT�A
�tA
�A	�{A	DgA�A;�AA?}A�xA?�A�MAo A�A��A<�AjA��A$�A \A ��@�+�@���@��@@��@���@��|@��
@���@�v`@�A @��@�i�@���@�M@�S&@�o@�~@�q@�!�@�K�@��@�@���@��@�X�@��}@�� @�Z�@��g@�/@ܦL@�e@ۥ@لM@־@ԦL@��@�zx@�w�@̏\@���@��@Ʌ@ȦL@�b@�%�@İ!@��@°!@���@��M@�GE@�iD@�Ft@��P@�~�@��~@��@�)_@���@���@�($@��Z@��V@�$t@�҉@�z@�֡@���@��@���@��L@���@�.I@��m@��'@�~(@�u@�ϫ@���@�L0@�)_@��.@�[W@���@���@�c�@�'R@�/@��@�i�@�f�@��@��@��@���@��B@�v�@�,=@�w2@��@��R@�h
@�3�@�U�@���@�&�@���@�z�@��@��3@�s@���@���@��L@�PH@���@���@�i�@�g8@�ff@�]d@��+@��@@�O@�v�@�PH@�1�@��;@���@�X@�P�@��f@�}V@��I@�\)@��]@��.@��P@�H�@�^�@�O�@�zx@�C-@��@�l�@���@�@���@���@�0�@�4n@�@O@��@��,@��@��v@��/@���@���@�2�@�;�@�;�@���@�b�@���@�]d@�<�@��@��0@�_p@��`@���@��@�@@�,=@��@��N@�$t@�Ɇ@���@�?�@���@���@��~@�Vm@���@��b@���@���@�hs@�U�@�e�@�e�@�a@�#�@�#�@�o�@�.I@���@��@��q@���@� �@��@�ԕ@���@�.I@��"@��[@��9@���@�@~��@}S&@}V@}@@|�O@|9X@{�V@z�@z�B@z�@z��@zxl@zs�@zQ@z
�@y�@yx�@y+�@x�v@x�e@xj@w��@wRT@v�@vd�@v�@u��@u��@u��@u�@t]d@tM@tZ@s�
@s8@r&�@q��@qVm@p[�@o��@ov`@oX�@oC�@o8@o,�@oC@nߤ@n)�@mc@m@l��@l7@k��@k��@k4�@j��@i�-@ie,@i2a@h��@h*�@g�@g�
@g��@g�@gK�@f�R@fs�@f\�@fC�@f)�@f�@f�@e��@e��@d��@dz�@d	�@c�$@cC@c�@c�@b�M@b}V@b!�@a \@`�p@`/�@_x@_!-@^��@^��@^-@]ϫ@]��@]a�@]X@]X@]Vm@]X@]L�@]A @]=�@]:�@]5�@\�E@\|�@\<�@\@[��@Z��@ZC�@Y��@YX@Xѷ@XU2@X�@W�$@Wn/@WS�@V�<@U�@U��@U�H@U�=@Uf�@U2a@U!�@T��@Tѷ@Tj@T,=@S��@S�4@S=@R��@R:*@Q�H@Q�@Q��@Q}�@Q&�@P��@P��@P��@Pw�@PXy@P/�@P*�@P%�@P�@Oƨ@Oo�@O33@N~�@M�D@M��@M�d@M��@M��@M�h@M��@M^�@M�@L��@L|�@LI�@L�@K��@KZ�@J��@Ju@I%F@H�9@HK^@H$@G�a@G]�@G!-@F��@F�X@F�F@F��@F\�@F?@F6�@F3�@F!�@F �@E�@E�z@E7L@D��@D]d@DM@D7@C�@C��@C1�@CY@B��@B�@A�N@A�@A-w@@�I@@*�@@�@?�@?t�@>��@>��@>n�@>Z�@>M�@=�T@=�t@=u�@<��@<��@<�4@<j@<M@;|�@;�@;�@:�@:�H@:�m@:�x@:��@:^5@:0U@:	@9��@9�-@9S&@98�@8�@8w�@8<�@7�@7��@7�@6�M@6�2@6�'@6}V@6.�@6�@6@5�'@54@4�@4��@4��@4]d@3��@3خ@3��@3/�@2�M@2�+@2{�@2l�@2h
@2Ta@2O@2
�@2u@1��@1�@1��@1��@1?}@1�@1�@0�@0�@0�o@0r�@0M@/�;@/��@/{J@/l�@/n/@/RT@/�@.�2@.\�@-}�@,�K@,ی@,ѷ@,�E@,�@,K^@+��@+˒@+��@+�@*�!@*s�@*Ov@*1�@*�@)�@)�-@)u�@)+�@(Ɇ@(�@(S�@(~@'خ@'�[@'��@'=@&��@&��@&z@&�@&�@%�j@%��@%c�@%5�@%%@$��@$|�@$I�@$/�@#�@#��@#�4@#O@#C@"�y@"�H@"�X@"�\@"\�@"�@!�Z@!�C@![W@!;@ Ɇ@ �@ 4n@��@C�@�@�@҉@�m@��@�x@�r@��@�@��@^5@B[@($@��@�S@hs@A @��@��@�5@�5@�`@��@��@��@e�@A�@7@�]@ݘ@�4@J#@�@�@�@͟@��@�A@p;@a|@W�@M�@J�@H�@B[@$�@�@��@u�@X@/@�@�@l"@x@qv@�@��@a|@;�@#:@�@�T@�@�@�#@�@�@�#@��@�9@�d@�C@|@s�@hs@O�@!�@�@�)@�_@>B@�m@�$@qv@E9@33@
=@��@� @^5@0U@-@�@��@��@��@�@7L@�@��@�@�I@q@A�@��@��@��@�[@��@�@@|�@F�@�@�@�@ں@��@��@�x@}V@M�@)�@�@�T@ϫ@�@m]@[W@0�@��@�?@��@�@j@2�@�
@��@C�@
�@
�X@
�@
��@
h
@
)�@
	@	�.@	ϫ@	�C@	�h@	0�@�)@��@]d@/�@x@�@�0@��@_p@>�@�@�@�,@�!@{�@GE@6�@&�@�@�.@��@�@��@�@w2@=�@@��@��@~(@z�@r�@A�@�]@��@�a@�[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�oB�oB��B�vB�AB�[B��B��B�AB�%B�[B5B�B�B�BpB�HB�B�B�pBҽB�pB��B��B��B�0B�B�XB�TB�-B�;B�B��B�B��B��B��B�B�AB��B��B��B}<Br�BqABo Bh>Ba-B^�BW�BR�BM�BHfB<�B5tB0;B.}B-CB+6B'�BB�B��B�B�8B��B��B�B�uB��B�MB�0B�BxBf�BQ�BGEB;B�B�B�/B�B��B�xB��B�KB��B��B�OBr�BfLB^�BOvBA�B1AB*�B"�BsB�B
��B
�`B
�B
��B
�B
��B
�)B
�uB
ɆB
��B
��B
�B
�xB
��B
��B
�gB
~�B
yrB
t�B
p!B
p�B
kkB
ezB
`'B
[WB
XB
T{B
NpB
@�B
)DB
]B
�B
�B
�B
B
 �B	��B	��B	�aB	�[B	�B	�B	�B	�B	�\B	�/B	�#B	�?B	ӏB	��B	�#B	ŢB	�UB	�HB	��B	��B	�`B	��B	��B	�sB	��B	��B	��B	�)B	�kB	��B	�,B	��B	��B	�B	�^B	��B	�_B	�3B	}B	z^B	x�B	u�B	sMB	o�B	m)B	jB	g8B	b�B	`�B	_VB	]�B	]dB	[�B	YB	RoB	P�B	P�B	P�B	O�B	K)B	FYB	CaB	@OB	>�B	<�B	9$B	1�B	-�B	+�B	)�B	(
B	#�B	#TB	!�B	!B	�B	�B	)B		B	+B	�B	uB	�B	NB	�B	�B	pB	�B	�B	
�B	fB	B	MB	�B	oB��B�BB�B�JB�DB��B�LB��B�B��B�|B�B�}B�]B�qB�B��B�B�B��B�B��B�B��B�HB��B�B�jB�OB�jB�~B�xB�CB��B��BؓB�BּB֡B��B�&B�&B��B� BϫB�B�B̳BˬB��B�rB��B��B��B�1BƎBżB�gB�?BāB��B�?B�YB�B�zB�#B��B�^B�B�B��B�"B�\B�\B��BЗB� B�hB�aB�BԯB�MBյB��B��BּB֡BںB��BޞB�B�HB��B�B�ZB�mB�B�yB�B��B�B�aB��B��B�fB�LB��B��B��B�HB	uB	�B	�B	�B	KB	fB	�B	
#B	�B	�B	.B	oB	aB	2B	B	IB	�B	$�B	'B	)�B	+6B	,�B	/�B	0�B	1AB	33B	5B	:�B	@ B	@4B	@iB	@�B	B�B	C�B	E�B	I�B	I�B	JrB	MB	PbB	R�B	WsB	`�B	n}B	r�B	yXB	�OB	��B	��B	�B	��B	��B	�QB	�B	�2B	��B	�QB	�B	�kB	��B	��B	�|B	�3B	��B	��B	�LB	�8B	��B	��B	��B	�B	�YB	��B	�~B	��B	��B	�B	��B	�
B	��B	�	B	��B	��B	�\B	�&B	�ZB	�TB	�B	��B	�HB	��B	�B	�pB	�B	ބB	ޞB	�pB	�B	�`B	�B	�B	�kB	�"B	�qB	�=B	�B	�]B	�B	��B	�5B	�aB	�GB	��B	�`B	��B	��B	��B	��B	��B	�jB	�"B	�BB
�B
B
%B
	�B
�B
HB
�B
MB
$B
�B
B
eB
�B
B
�B
�B
�B
!-B
"�B
%B
&�B
'�B
*eB
-CB
.�B
1'B
2�B
4�B
5�B
6FB
8�B
;0B
;�B
;�B
=<B
?B
DMB
F%B
G�B
LB
N�B
O�B
PbB
P�B
Q B
Q4B
QNB
R:B
U2B
WsB
X�B
[	B
\]B
]�B
^B
^�B
`�B
d�B
fB
g�B
i�B
lWB
l�B
mCB
nB
n�B
o�B
r|B
s�B
t�B
uZB
vB
vFB
vFB
v�B
wfB
y�B
{0B
|�B
~B
HB
}B
cB
�B
�B
��B
�B
��B
��B
�=B
�xB
�~B
�6B
��B
��B
��B
��B
��B
��B
��B
��B
��B
� B
�:B
�:B
� B
��B
�{B
�gB
��B
�?B
�QB
��B
�~B
�jB
�BB
��B
�hB
��B
�B
�&B
��B
�XB
��B
��B
�_B
�B
�B
��B
�B
��B
��B
�)B
��B
� B
��B
�AB
�aB
��B
��B
��B
�nB
�ZB
��B
�zB
��B
�2B
��B
��B
�B
��B
�B
��B
��B
�*B
�B
�VB
�qB
��B
��B
�(B
�BB
�]B
��B
�cB
�iB
��B
�;B
��B
B
�GB
ĶB
�?B
ȚB
ɆB
�XB
ʦB
ˬB
̘B
�6B
�PB
͹B
�B
�"B
ΊB
οB
��B
��B
��B
�BB
�\B
ϑB
��B
�B
�:B
�:B
��B
��B
��B
ԕB
��B
�SB
�
B
�sB
��B
خB
��B
ںB
��B
�	B
�)B
�B
�B
�B
�5B
�5B
�;B
�VB
��B
��B
�-B
�bB
��B
�hB
�B
�&B
�ZB
�ZB
�B
��B
�B
�B
�B
��B
��B
�LB
�B
�B
�B
�$B
��B
�yB
��B
�B
�B
�B
�B
�B
�B
�B
�B
�CB
�B
��B
�OB
�OB
�B
�;B
�B
�'B
�[B
�aB
�B
�hB
�hB
�B
�B
�B
�B
�9B
�9B
�TB
�nB
��B
�B
��B
��B
��B
�zB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�$B
�	B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
�HB
��B B iB iB �B �B �BUB�B�BBaB�B�B3B�B�B�B�B%B�B�B+BzB�B�BfBfB	7B	lB	�B	�B
XB
�B
�BDBxBxB�B�BJB�B�BPB�BVB�B�B\BHB B4BhB�B�B�BB B:B B:BTB�B�B@B�B�BB�B�B�B�B�B�BBMBgB�B�BBB�B�B?BYB?B�B+B+B_ByByB�B�B�B�B�BBB�B�BB7B�B#B�B]B�B~BBBOB�B�B�B�B�B�B�B�B�B�BB;B�B�B�B�B 'B B �B �B!bB!�B"4B"hB"�B"�B"�B#nB#�B#�B$@B$&B$�B$tB$�B$�B$�B%zB&B&LB&LB&fB&�B&�B'�B'�B'�B'�B'�B'�B'�B(XB(�B(�B(�B(�B)B)DB)DB)yB)�B)�B*0B*KB*eB*�B+B+B+kB+�B+�B,B,=B,WB,�B-)B-wB-�B.}B.�B.�B.�B/5B/�B/�B/�B0B0!B0;B0�B1[B1�B2B2GB2|B2�B2�B3MB3hB3�B3�B4TB4nB4TB4�B5%B5%B5?B5?B5�B5�B5�B5�B5�B6`B6�B6�B72B7�B7�B7�B7�B8B8�B8�B8�B8�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B�oB�oB��B�vB�AB�[B��B��B�AB�%B�[B5B�B�B�BpB�HB�B�B�pBҽB�pB��B��B��B�0B�B�XB�TB�-B�;B�B��B�B��B��B��B�B�AB��B��B��B}<Br�BqABo Bh>Ba-B^�BW�BR�BM�BHfB<�B5tB0;B.}B-CB+6B'�BB�B��B�B�8B��B��B�B�uB��B�MB�0B�BxBf�BQ�BGEB;B�B�B�/B�B��B�xB��B�KB��B��B�OBr�BfLB^�BOvBA�B1AB*�B"�BsB�B
��B
�`B
�B
��B
�B
��B
�)B
�uB
ɆB
��B
��B
�B
�xB
��B
��B
�gB
~�B
yrB
t�B
p!B
p�B
kkB
ezB
`'B
[WB
XB
T{B
NpB
@�B
)DB
]B
�B
�B
�B
B
 �B	��B	��B	�aB	�[B	�B	�B	�B	�B	�\B	�/B	�#B	�?B	ӏB	��B	�#B	ŢB	�UB	�HB	��B	��B	�`B	��B	��B	�sB	��B	��B	��B	�)B	�kB	��B	�,B	��B	��B	�B	�^B	��B	�_B	�3B	}B	z^B	x�B	u�B	sMB	o�B	m)B	jB	g8B	b�B	`�B	_VB	]�B	]dB	[�B	YB	RoB	P�B	P�B	P�B	O�B	K)B	FYB	CaB	@OB	>�B	<�B	9$B	1�B	-�B	+�B	)�B	(
B	#�B	#TB	!�B	!B	�B	�B	)B		B	+B	�B	uB	�B	NB	�B	�B	pB	�B	�B	
�B	fB	B	MB	�B	oB��B�BB�B�JB�DB��B�LB��B�B��B�|B�B�}B�]B�qB�B��B�B�B��B�B��B�B��B�HB��B�B�jB�OB�jB�~B�xB�CB��B��BؓB�BּB֡B��B�&B�&B��B� BϫB�B�B̳BˬB��B�rB��B��B��B�1BƎBżB�gB�?BāB��B�?B�YB�B�zB�#B��B�^B�B�B��B�"B�\B�\B��BЗB� B�hB�aB�BԯB�MBյB��B��BּB֡BںB��BޞB�B�HB��B�B�ZB�mB�B�yB�B��B�B�aB��B��B�fB�LB��B��B��B�HB	uB	�B	�B	�B	KB	fB	�B	
#B	�B	�B	.B	oB	aB	2B	B	IB	�B	$�B	'B	)�B	+6B	,�B	/�B	0�B	1AB	33B	5B	:�B	@ B	@4B	@iB	@�B	B�B	C�B	E�B	I�B	I�B	JrB	MB	PbB	R�B	WsB	`�B	n}B	r�B	yXB	�OB	��B	��B	�B	��B	��B	�QB	�B	�2B	��B	�QB	�B	�kB	��B	��B	�|B	�3B	��B	��B	�LB	�8B	��B	��B	��B	�B	�YB	��B	�~B	��B	��B	�B	��B	�
B	��B	�	B	��B	��B	�\B	�&B	�ZB	�TB	�B	��B	�HB	��B	�B	�pB	�B	ބB	ޞB	�pB	�B	�`B	�B	�B	�kB	�"B	�qB	�=B	�B	�]B	�B	��B	�5B	�aB	�GB	��B	�`B	��B	��B	��B	��B	��B	�jB	�"B	�BB
�B
B
%B
	�B
�B
HB
�B
MB
$B
�B
B
eB
�B
B
�B
�B
�B
!-B
"�B
%B
&�B
'�B
*eB
-CB
.�B
1'B
2�B
4�B
5�B
6FB
8�B
;0B
;�B
;�B
=<B
?B
DMB
F%B
G�B
LB
N�B
O�B
PbB
P�B
Q B
Q4B
QNB
R:B
U2B
WsB
X�B
[	B
\]B
]�B
^B
^�B
`�B
d�B
fB
g�B
i�B
lWB
l�B
mCB
nB
n�B
o�B
r|B
s�B
t�B
uZB
vB
vFB
vFB
v�B
wfB
y�B
{0B
|�B
~B
HB
}B
cB
�B
�B
��B
�B
��B
��B
�=B
�xB
�~B
�6B
��B
��B
��B
��B
��B
��B
��B
��B
��B
� B
�:B
�:B
� B
��B
�{B
�gB
��B
�?B
�QB
��B
�~B
�jB
�BB
��B
�hB
��B
�B
�&B
��B
�XB
��B
��B
�_B
�B
�B
��B
�B
��B
��B
�)B
��B
� B
��B
�AB
�aB
��B
��B
��B
�nB
�ZB
��B
�zB
��B
�2B
��B
��B
�B
��B
�B
��B
��B
�*B
�B
�VB
�qB
��B
��B
�(B
�BB
�]B
��B
�cB
�iB
��B
�;B
��B
B
�GB
ĶB
�?B
ȚB
ɆB
�XB
ʦB
ˬB
̘B
�6B
�PB
͹B
�B
�"B
ΊB
οB
��B
��B
��B
�BB
�\B
ϑB
��B
�B
�:B
�:B
��B
��B
��B
ԕB
��B
�SB
�
B
�sB
��B
خB
��B
ںB
��B
�	B
�)B
�B
�B
�B
�5B
�5B
�;B
�VB
��B
��B
�-B
�bB
��B
�hB
�B
�&B
�ZB
�ZB
�B
��B
�B
�B
�B
��B
��B
�LB
�B
�B
�B
�$B
��B
�yB
��B
�B
�B
�B
�B
�B
�B
�B
�B
�CB
�B
��B
�OB
�OB
�B
�;B
�B
�'B
�[B
�aB
�B
�hB
�hB
�B
�B
�B
�B
�9B
�9B
�TB
�nB
��B
�B
��B
��B
��B
�zB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�$B
�	B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
�HB
��B B iB iB �B �B �BUB�B�BBaB�B�B3B�B�B�B�B%B�B�B+BzB�B�BfBfB	7B	lB	�B	�B
XB
�B
�BDBxBxB�B�BJB�B�BPB�BVB�B�B\BHB B4BhB�B�B�BB B:B B:BTB�B�B@B�B�BB�B�B�B�B�B�BBMBgB�B�BBB�B�B?BYB?B�B+B+B_ByByB�B�B�B�B�BBB�B�BB7B�B#B�B]B�B~BBBOB�B�B�B�B�B�B�B�B�B�BB;B�B�B�B�B 'B B �B �B!bB!�B"4B"hB"�B"�B"�B#nB#�B#�B$@B$&B$�B$tB$�B$�B$�B%zB&B&LB&LB&fB&�B&�B'�B'�B'�B'�B'�B'�B'�B(XB(�B(�B(�B(�B)B)DB)DB)yB)�B)�B*0B*KB*eB*�B+B+B+kB+�B+�B,B,=B,WB,�B-)B-wB-�B.}B.�B.�B.�B/5B/�B/�B/�B0B0!B0;B0�B1[B1�B2B2GB2|B2�B2�B3MB3hB3�B3�B4TB4nB4TB4�B5%B5%B5?B5?B5�B5�B5�B5�B5�B6`B6�B6�B72B7�B7�B7�B7�B8B8�B8�B8�B8�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230701035128  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230701035129  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230701035130  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230701035130                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230701035131  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230701035131  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230701043044                      G�O�G�O�G�O�                