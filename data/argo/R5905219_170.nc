CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-26T12:41:52Z creation;2022-11-26T12:41:54Z conversion to V3.1      
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221126124152  20221126125753  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @� хOC�1   @� �O��P@3��E����c��-1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B7��B?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C�C�C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� DfD�fDfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(y�D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DGy�DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dny�Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dy��Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D���D�<�D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D��3D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�3D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ D�|�D�� D�  D�@ Dʀ D�� D�  D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�@ D̀ D��3D�  D�@ D΀ D�� D�  D�C3Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�|�D���D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D���D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�3D�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B*  B1��B934BA34BI34BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���C L�CffCffCffCffC
ffCffCffCffCffCffCffC� C� CffCffC ffC"ffC$L�C&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6L�C8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�@ C�@ C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�&fC�&fC�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�&fC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33D �D ��D�D��D�D��D  D� D  D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D� D�D��D4D��D�D��D�D��D�D��D�D��D�D��D�D��D4D��D�D��D�D��D  D� D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&� D'�D'��D(�D(�4D)�D)��D*�D*��D+�D+��D,  D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1� D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7  D7� D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG4DG�4DH4DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ  DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl�4Dm�Dm��Dn�Dn�4Do�Do��Dp�Dp�4Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz4Dz�4D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�L�D���D���D��D�L�D���D���D� D�L�D���D���D��D�L�D���D���D��D�L�D���D�� D� D�L�D���D���D��D�L�D���D���D��D�L�D���D���D�	�D�L�D���D���D��D�L�D���D���D��D�L�D���D���D� D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D�� D�� D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D�� D���D��D�L�D�� D�� D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�I�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�I�D���D���D�	�D�I�D���D���D��D�L�D���D�� D��D�L�D���D�� D��D�P D�� D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D�� D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�P D�� D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�I�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D�D���D� D�L�DÌ�D���D��D�L�DČ�D���D��D�L�DŌ�D���D��D�L�Dƌ�D���D��D�L�Dǌ�D���D��D�L�DȌ�D���D��D�L�Dɉ�D���D��D�L�Dʌ�D���D��D�L�Dˉ�D���D��D�L�Ď�D���D��D�L�D͌�D�� D��D�L�DΌ�D���D��D�P Dό�D���D��D�L�DЌ�D���D��D�L�Dь�D���D��D�L�DҌ�D���D��D�L�Dӌ�D���D��D�L�DԌ�D���D��D�L�DՌ�D���D��D�L�D֌�D���D��D�L�D׌�D���D��D�L�D،�D���D��D�L�Dٌ�D���D��D�L�Dڌ�D���D��D�L�Dی�D���D��D�L�D܌�D���D��D�L�D݌�D���D��D�L�Dތ�D���D��D�L�Dߌ�D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D䉚D���D��D�L�D��D���D��D�L�D扚D���D�	�D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�I�D��D���D��D�L�D뉚D���D��D�L�D��D���D��D�L�D퉚D�ɚD��D�L�D��D���D��D�L�D��D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�P D��D���D�	�D�L�D���D���D� D�P D���D���D��D�L�D���D���D� D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�ȴA���A��;A��;A��;A��;A��HA��;A��HA��TA��HA��TA��`A��yA��A��A��yA��yA��A��A��A��yA��yA��A���A���A���A���A���A���A���A���A���A���A��A��yA��mA��;A���A���A���A�ƨA�A�A��mA�t�A�
=A�S�A�33A��A�
=A�Q�A�VA��A�{AøRA���A��
A�G�A��7A���A���A��hA�&�A�XA��A�(�A�r�A�A�VA��wA�(�A��mA�|�A��A��#A�1'A��FA�C�A��+A��9A�VA�^5A���A��A��A��/A��A�JA���A�z�A�O�A��TA�z�A��
A��A���A��#A�
=A��A�G�A���A���A���A�
=A�A��RA� �A��`A���A�x�A|5?Az�Az�Ay�AxjAw7LAv�`AsdZApZAn{Am�Al��AlffAi�mAg�FAdz�Ac��AbVAa�FAa�A_�A\��A[��A[x�A[�AZ�RAZ  AY;dAW�AWdZAU�#AT�ARE�AP�AK/AHAC&�A?hsA=�A:VA7�
A5�PA4E�A3�A3�A1��A1?}A0�A0�DA/��A-�TA*9XA(v�A'\)A&�HA&��A%�wA$��A$E�A$9XA"�jA!�A (�A�wA��AVAQ�A��A��A�mAn�A�Al�AO�A�A
=AK�AO�A��A��AdZAK�AhsA��A�A��A�hA
9XA	�^A	x�Az�A�;A`BAv�A/A�A�A��A�A�!AJA��A �HA �A ^5@�C�@�~�@�{@��@��7@�I�@�~�@���@��9@��y@��\@�E�@�/@�@�?}@�@��@�^5@��@��m@��@�33@�R@�^5@��T@��@� �@�  @�l�@�n�@�%@㝲@�M�@�$�@�h@�r�@�
=@�^5@���@��`@�
=@�X@� �@��;@׍P@�+@֗�@�@ԃ@��@Ӆ@�|�@�;d@��@�n�@�J@�V@У�@�"�@���@Χ�@�V@�@��#@͙�@���@�Ĝ@̛�@�r�@˕�@��@�M�@��@ɩ�@��/@�9X@ǅ@�@�ff@�-@�@�G�@ě�@öF@�l�@�C�@��H@�=q@�@�%@��`@��@�I�@��;@��P@��@��^@�`B@�7L@�V@��/@�z�@�(�@��;@�K�@��H@��H@��7@���@��;@�|�@�^5@�S�@�v�@�^5@���@���@��^@�p�@�(�@�5?@���@���@�{@�x�@���@�A�@�  @�t�@�t�@�t�@�t�@�l�@���@�=q@���@�bN@�`B@���@��@��@���@�ȴ@��!@�E�@��h@�/@��`@��@��9@��@�$�@�J@�@��7@�p�@�x�@���@�Q�@�A�@�9X@�1'@�b@��m@�K�@�o@��@���@�ȴ@��+@��\@��@��^@���@��7@�x�@�&�@���@���@��^@�7L@�?}@�?}@��@��@�9X@��
@��w@��@���@�t�@�"�@�v�@��#@��h@�X@�Z@���@�;d@�33@��@�E�@���@��^@�x�@���@���@�I�@�ƨ@�C�@���@��@��H@���@�ȴ@��R@��!@��R@���@�~�@�J@�p�@��@��@��@�V@���@��@�9X@��;@�@���@�V@�E�@�-@�{@�J@��@��T@���@���@��/@�r�@�Z@�Q�@�I�@�I�@�I�@�A�@�9X@�(�@�1@�|�@�\)@�\)@�33@�"�@�o@�@��@���@�ff@�^5@�{@��T@��-@��h@�?}@���@���@�Z@��@���@�l�@��@�^5@�J@���@���@��@�X@�/@��@���@���@�  @��@�S�@�
=@��@���@�n�@��T@��@�X@�/@�Ĝ@�A�@��@��@�w@�w@�@\)@+@~��@}�T@|�@|�D@|�D@|Z@{��@z�H@z��@zM�@z-@z�@zJ@y��@y��@y�#@yx�@yG�@x��@x�u@xA�@xb@w�P@w;d@w
=@v�y@vV@u@u�@up�@u`B@uO�@uO�@u/@uV@t�@t�j@s33@rn�@q��@qhs@q%@p��@pbN@o�@o�P@oK�@o�@n�y@nV@m��@mO�@l��@l(�@l1@k�
@kt�@k@j�!@i��@i&�@h��@g�@gl�@gK�@g
=@f�@f��@f��@f��@f��@f�+@fv�@f5?@e@ep�@d�@d��@dz�@dI�@d�@co@a�7@`��@`�u@`A�@`  @_�;@_�;@_�;@_\)@^�y@^��@^V@]��@]/@\�@\(�@Z�H@Y��@Y�@Y�7@X��@X�`@X�`@X��@X��@X��@X1'@W�w@W�@W�P@WK�@W�@V�y@V��@V5?@U�@U�-@UV@S��@S@Rn�@Q�7@QG�@P�u@P  @O�w@O�P@Ol�@O
=@N�R@N{@M@Mp�@M/@L��@L�@L�j@L�D@Lj@L(�@Kƨ@Kt�@KS�@K33@J�H@J��@J^5@J�@I�@I�^@Ihs@H�9@Hb@G�w@G;d@F�@Fff@E��@E`B@E�@D�@D��@Dz�@D�@C�
@C��@C�@CS�@B�H@B��@Bn�@Bn�@B^5@BM�@A��@Ahs@A7L@A&�@A%@@�`@@Ĝ@@�u@@b@?|�@?+@>��@>�y@>��@>��@>V@>@=��@=�@=�@=�@=V@<j@;�m@;��@;��@;�@:�@:J@9�^@9hs@9G�@9%@8��@8�9@8��@8�@8  @7;d@6�@6V@5�@5�@5�@6@6@5�-@5�-@5��@5`B@5/@4�@4�D@4I�@3�m@3S�@2�H@2��@2�\@1�^@1%@0�u@01'@0 �@0 �@/�;@/��@/;d@.��@.�@/+@/�@/�;@/��@/
=@.�y@.�R@.ff@.V@.E�@.{@-@-@-@-��@-p�@-?}@-?}@-O�@-�@,�@,�j@,��@,�D@,j@,I�@,9X@,�@,1@+�m@+��@+C�@*��@*n�@*=q@*�@)�@)�#@)��@)��@)��@)�7@)&�@(bN@(b@'��@'�@'|�@';d@&��@&�R@&��@&��@&�+@&V@&{@%��@%�@%`B@%V@$�j@$�D@$j@$j@$I�@$1@#t�@#"�@"�@"��@"�\@"n�@"M�@"-@"�@!��@!�^@!x�@!X@!�@ ��@ �`@ �`@ Ĝ@ bN@ b@l�@K�@;d@;d@+@�y@�R@�+@E�@�@��@��@@�-@�@`B@`B@�@V@��@�j@(�@�m@ƨ@ƨ@ƨ@dZ@33@33@33@�H@�!@n�@=q@-@J@��@�7@x�@%@�u@bN@Q�@A�@ �@�@��@�w@�@��@;d@�@��@��@V@5?@5?@{@�T@��@/@�@�/@�D@9X@�@��@33@o@�H@��@�\@n�@^5@=q@�@�^@X@&�@%@Ĝ@�9@��@�@r�@Q�@  @��@l�@�@�@
=@�y@��@v�@E�@�@��@�h@`B@O�@/@V@��@�@�@��@��@Z@9X@1@��@��@�m@�m@ƨ@ƨ@��@t�@dZ@dZ@S�@S�@C�@"�@
�H@
��@
�!@
^5@
=q@
J@	��@	�^@	��@	�7@	hs@	7L@	&�@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�ȴA���A��;A��;A��;A��;A��HA��;A��HA��TA��HA��TA��`A��yA��A��A��yA��yA��A��A��A��yA��yA��A���A���A���A���A���A���A���A���A���A���A��A��yA��mA��;A���A���A���A�ƨA�A�A��mA�t�A�
=A�S�A�33A��A�
=A�Q�A�VA��A�{AøRA���A��
A�G�A��7A���A���A��hA�&�A�XA��A�(�A�r�A�A�VA��wA�(�A��mA�|�A��A��#A�1'A��FA�C�A��+A��9A�VA�^5A���A��A��A��/A��A�JA���A�z�A�O�A��TA�z�A��
A��A���A��#A�
=A��A�G�A���A���A���A�
=A�A��RA� �A��`A���A�x�A|5?Az�Az�Ay�AxjAw7LAv�`AsdZApZAn{Am�Al��AlffAi�mAg�FAdz�Ac��AbVAa�FAa�A_�A\��A[��A[x�A[�AZ�RAZ  AY;dAW�AWdZAU�#AT�ARE�AP�AK/AHAC&�A?hsA=�A:VA7�
A5�PA4E�A3�A3�A1��A1?}A0�A0�DA/��A-�TA*9XA(v�A'\)A&�HA&��A%�wA$��A$E�A$9XA"�jA!�A (�A�wA��AVAQ�A��A��A�mAn�A�Al�AO�A�A
=AK�AO�A��A��AdZAK�AhsA��A�A��A�hA
9XA	�^A	x�Az�A�;A`BAv�A/A�A�A��A�A�!AJA��A �HA �A ^5@�C�@�~�@�{@��@��7@�I�@�~�@���@��9@��y@��\@�E�@�/@�@�?}@�@��@�^5@��@��m@��@�33@�R@�^5@��T@��@� �@�  @�l�@�n�@�%@㝲@�M�@�$�@�h@�r�@�
=@�^5@���@��`@�
=@�X@� �@��;@׍P@�+@֗�@�@ԃ@��@Ӆ@�|�@�;d@��@�n�@�J@�V@У�@�"�@���@Χ�@�V@�@��#@͙�@���@�Ĝ@̛�@�r�@˕�@��@�M�@��@ɩ�@��/@�9X@ǅ@�@�ff@�-@�@�G�@ě�@öF@�l�@�C�@��H@�=q@�@�%@��`@��@�I�@��;@��P@��@��^@�`B@�7L@�V@��/@�z�@�(�@��;@�K�@��H@��H@��7@���@��;@�|�@�^5@�S�@�v�@�^5@���@���@��^@�p�@�(�@�5?@���@���@�{@�x�@���@�A�@�  @�t�@�t�@�t�@�t�@�l�@���@�=q@���@�bN@�`B@���@��@��@���@�ȴ@��!@�E�@��h@�/@��`@��@��9@��@�$�@�J@�@��7@�p�@�x�@���@�Q�@�A�@�9X@�1'@�b@��m@�K�@�o@��@���@�ȴ@��+@��\@��@��^@���@��7@�x�@�&�@���@���@��^@�7L@�?}@�?}@��@��@�9X@��
@��w@��@���@�t�@�"�@�v�@��#@��h@�X@�Z@���@�;d@�33@��@�E�@���@��^@�x�@���@���@�I�@�ƨ@�C�@���@��@��H@���@�ȴ@��R@��!@��R@���@�~�@�J@�p�@��@��@��@�V@���@��@�9X@��;@�@���@�V@�E�@�-@�{@�J@��@��T@���@���@��/@�r�@�Z@�Q�@�I�@�I�@�I�@�A�@�9X@�(�@�1@�|�@�\)@�\)@�33@�"�@�o@�@��@���@�ff@�^5@�{@��T@��-@��h@�?}@���@���@�Z@��@���@�l�@��@�^5@�J@���@���@��@�X@�/@��@���@���@�  @��@�S�@�
=@��@���@�n�@��T@��@�X@�/@�Ĝ@�A�@��@��@�w@�w@�@\)@+@~��@}�T@|�@|�D@|�D@|Z@{��@z�H@z��@zM�@z-@z�@zJ@y��@y��@y�#@yx�@yG�@x��@x�u@xA�@xb@w�P@w;d@w
=@v�y@vV@u@u�@up�@u`B@uO�@uO�@u/@uV@t�@t�j@s33@rn�@q��@qhs@q%@p��@pbN@o�@o�P@oK�@o�@n�y@nV@m��@mO�@l��@l(�@l1@k�
@kt�@k@j�!@i��@i&�@h��@g�@gl�@gK�@g
=@f�@f��@f��@f��@f��@f�+@fv�@f5?@e@ep�@d�@d��@dz�@dI�@d�@co@a�7@`��@`�u@`A�@`  @_�;@_�;@_�;@_\)@^�y@^��@^V@]��@]/@\�@\(�@Z�H@Y��@Y�@Y�7@X��@X�`@X�`@X��@X��@X��@X1'@W�w@W�@W�P@WK�@W�@V�y@V��@V5?@U�@U�-@UV@S��@S@Rn�@Q�7@QG�@P�u@P  @O�w@O�P@Ol�@O
=@N�R@N{@M@Mp�@M/@L��@L�@L�j@L�D@Lj@L(�@Kƨ@Kt�@KS�@K33@J�H@J��@J^5@J�@I�@I�^@Ihs@H�9@Hb@G�w@G;d@F�@Fff@E��@E`B@E�@D�@D��@Dz�@D�@C�
@C��@C�@CS�@B�H@B��@Bn�@Bn�@B^5@BM�@A��@Ahs@A7L@A&�@A%@@�`@@Ĝ@@�u@@b@?|�@?+@>��@>�y@>��@>��@>V@>@=��@=�@=�@=�@=V@<j@;�m@;��@;��@;�@:�@:J@9�^@9hs@9G�@9%@8��@8�9@8��@8�@8  @7;d@6�@6V@5�@5�@5�@6@6@5�-@5�-@5��@5`B@5/@4�@4�D@4I�@3�m@3S�@2�H@2��@2�\@1�^@1%@0�u@01'@0 �@0 �@/�;@/��@/;d@.��@.�@/+@/�@/�;@/��@/
=@.�y@.�R@.ff@.V@.E�@.{@-@-@-@-��@-p�@-?}@-?}@-O�@-�@,�@,�j@,��@,�D@,j@,I�@,9X@,�@,1@+�m@+��@+C�@*��@*n�@*=q@*�@)�@)�#@)��@)��@)��@)�7@)&�@(bN@(b@'��@'�@'|�@';d@&��@&�R@&��@&��@&�+@&V@&{@%��@%�@%`B@%V@$�j@$�D@$j@$j@$I�@$1@#t�@#"�@"�@"��@"�\@"n�@"M�@"-@"�@!��@!�^@!x�@!X@!�@ ��@ �`@ �`@ Ĝ@ bN@ b@l�@K�@;d@;d@+@�y@�R@�+@E�@�@��@��@@�-@�@`B@`B@�@V@��@�j@(�@�m@ƨ@ƨ@ƨ@dZ@33@33@33@�H@�!@n�@=q@-@J@��@�7@x�@%@�u@bN@Q�@A�@ �@�@��@�w@�@��@;d@�@��@��@V@5?@5?@{@�T@��@/@�@�/@�D@9X@�@��@33@o@�H@��@�\@n�@^5@=q@�@�^@X@&�@%@Ĝ@�9@��@�@r�@Q�@  @��@l�@�@�@
=@�y@��@v�@E�@�@��@�h@`B@O�@/@V@��@�@�@��@��@Z@9X@1@��@��@�m@�m@ƨ@ƨ@��@t�@dZ@dZ@S�@S�@C�@"�@
�H@
��@
�!@
^5@
=q@
J@	��@	�^@	��@	�7@	hs@	7L@	&�@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBB  B+B\)BXBz�BÖBƨBĜBÖB��B�jB�'B��B��B�oB�uB�uB�VB�+B�B�%B�\B�{B��B�!B��B�VB|�Bm�BS�BJ�BA�B;dB7LB/B$�B�BuBPB+BB��B�mB�BB�BǮB�jB�RB��B��B��B�bB�%B{�B`BBR�BH�B>wB%�BB
�sB
ɺB
�dB
�B
��B
�hB
� B
p�B
`BB
D�B
5?B
1'B
-B
$�B
�B
{B
1B	�B	�NB	�B	�
B	��B	ǮB	�jB	��B	��B	��B	�{B	�\B	�7B	z�B	u�B	s�B	p�B	o�B	jB	hsB	aHB	]/B	W
B	N�B	E�B	8RB	$�B	oB	1B��B�B�yB�`B�BB�)B�#B�B�
B��B��B��B��B��BǮBǮB��BɺBǮB�5B�mB�ZB�yB��B��B	B	B	B	B	B��B	B	+B	1B	%B	B	B	B	  B��B�B�)B��B��B��B��B�B�/B�B��BĜB�wB��B�jB�^B�XB�jB�jB�dB�jB�jB�qB�}B�}B��BÖBBBBĜBƨBƨBƨBǮBǮBƨB��B��B��B��B��B��B��B��B��B�B�
B�B�
B�B�B�B�B�#B�#B�#B�/B�;B�NB�ZB�`B�ZB�`B�mB�yB�yB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	+B	JB	PB	PB	VB	\B	\B	bB	uB	uB	uB	uB	�B	�B	�B	�B	!�B	$�B	%�B	(�B	)�B	-B	-B	/B	0!B	33B	7LB	8RB	8RB	:^B	>wB	A�B	D�B	D�B	E�B	G�B	H�B	I�B	L�B	P�B	Q�B	R�B	S�B	T�B	W
B	ZB	]/B	aHB	dZB	k�B	�B	�hB	�VB	�uB	�uB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	�XB	�^B	�wB	��B	��B	��B	��B	ŢB	��B	ɺB	��B	�
B	�B	�B	�)B	�/B	�/B	�/B	�/B	�)B	�)B	�;B	�BB	�HB	�HB	�BB	�NB	�`B	�fB	�fB	�mB	�B	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
%B
1B
JB
JB
VB
VB
\B
bB
\B
hB
oB
uB
uB
uB
{B
�B
�B
�B
{B
{B
uB
oB
oB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
!�B
!�B
"�B
"�B
#�B
#�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
-B
.B
-B
-B
.B
.B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
>wB
=qB
>wB
>wB
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
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
J�B
I�B
I�B
J�B
J�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
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
T�B
VB
VB
W
B
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
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
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
bNB
cTB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
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
gmB
hsB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
o�B
n�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
r�B
t�B
u�B
u�B
v�B
u�B
v�B
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
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
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
��B
��B
��B
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBB  B+B\)BXBz�BÖBƨBĜBÖB��B�jB�'B��B��B�oB�uB�uB�VB�+B�B�%B�\B�{B��B�!B��B�VB|�Bm�BS�BJ�BA�B;dB7LB/B$�B�BuBPB+BB��B�mB�BB�BǮB�jB�RB��B��B��B�bB�%B{�B`BBR�BH�B>wB%�BB
�sB
ɺB
�dB
�B
��B
�hB
� B
p�B
`BB
D�B
5?B
1'B
-B
$�B
�B
{B
1B	�B	�NB	�B	�
B	��B	ǮB	�jB	��B	��B	��B	�{B	�\B	�7B	z�B	u�B	s�B	p�B	o�B	jB	hsB	aHB	]/B	W
B	N�B	E�B	8RB	$�B	oB	1B��B�B�yB�`B�BB�)B�#B�B�
B��B��B��B��B��BǮBǮB��BɺBǮB�5B�mB�ZB�yB��B��B	B	B	B	B	B��B	B	+B	1B	%B	B	B	B	  B��B�B�)B��B��B��B��B�B�/B�B��BĜB�wB��B�jB�^B�XB�jB�jB�dB�jB�jB�qB�}B�}B��BÖBBBBĜBƨBƨBƨBǮBǮBƨB��B��B��B��B��B��B��B��B��B�B�
B�B�
B�B�B�B�B�#B�#B�#B�/B�;B�NB�ZB�`B�ZB�`B�mB�yB�yB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	B	+B	JB	PB	PB	VB	\B	\B	bB	uB	uB	uB	uB	�B	�B	�B	�B	!�B	$�B	%�B	(�B	)�B	-B	-B	/B	0!B	33B	7LB	8RB	8RB	:^B	>wB	A�B	D�B	D�B	E�B	G�B	H�B	I�B	L�B	P�B	Q�B	R�B	S�B	T�B	W
B	ZB	]/B	aHB	dZB	k�B	�B	�hB	�VB	�uB	�uB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�FB	�XB	�^B	�wB	��B	��B	��B	��B	ŢB	��B	ɺB	��B	�
B	�B	�B	�)B	�/B	�/B	�/B	�/B	�)B	�)B	�;B	�BB	�HB	�HB	�BB	�NB	�`B	�fB	�fB	�mB	�B	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
%B
1B
JB
JB
VB
VB
\B
bB
\B
hB
oB
uB
uB
uB
{B
�B
�B
�B
{B
{B
uB
oB
oB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
!�B
!�B
"�B
"�B
#�B
#�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
-B
.B
-B
-B
.B
.B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
>wB
=qB
>wB
>wB
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
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
J�B
I�B
I�B
J�B
J�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
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
T�B
VB
VB
W
B
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
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
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
bNB
cTB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
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
gmB
hsB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
o�B
n�B
o�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
r�B
t�B
u�B
u�B
v�B
u�B
v�B
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
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�{B
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
��B
��B
��B
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20221126214112  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221126124152  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221126124152  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221126124154                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221126124154  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221126124154  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221126125753                      G�O�G�O�G�O�                