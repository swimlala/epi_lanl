CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:24:54Z creation;2022-06-04T19:24:54Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192454  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               RA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�s���I21   @�s�dPg)@+ۥ�S���d�t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�33@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  BÙ�B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF�CG�fCJ  CL  CN�CP  CR  CS�fCU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl�Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@)��@���@�ff@�ffA33A?33A]��A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B@33BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��fBÀ B��fB˳3B��fB��fB��fB��fB��fB��fB��fB�3B��fB��B��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CD�CF�CGٙCI�3CK�3CN�CO�3CQ�3CSٙCUٙCW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Cj�Cl�CmٙCo�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ�3DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Dv3Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��3D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�7�A�8RA�9�A�9$A��A��A���A��(A���A��A��-A˙1A�t�A�b�A�\�A�XEA�T�A�P�A�L�A�IRA�DgA�B'A�:^A�/�A���A��AʖA�j�Aɨ�A�#nA���A��jA�ɺAȥFA�}�A�j�A�LdA���A�PHA�A��A��A���AƘ�A�1[A��AœA�A�U�A��A��A�f�A��+A�<�A�zA���A�g8A���A��A�gmA��0A��A�e�A��OA�y>A���A���A��[A�;�A��XA��A��%A�^A�A�q�A��A�ٴA�:*A��LA��vA���A�8�A���A�
�A�(�A�S�A��A���A�ɺA��A��A��A��A��A��&A�רA�{A���A�1A��4A��oA��=A���A���Ay�$As��Am($Ai�Ag�HAg6zAa�/A\|�AW��AS�ANXyAJ��AH��AF�ACS�A@tTA?�LA>hsA=>�A<��A=;�A<j�A;VmA:]dA9K^A7 �A5�A4�_A4A3��A3]�A2�+A2�rA2?A2Q�A1�\A0W?A.�fA. �A,a�A,�A,�
A,��A,�-A,C-A+�A+�A*xA)6A)D�A)JA'�qA&�,A&�A'�A&��A%&�A%�A%�DA&�"A&��A%�8A$��A$@OA#'RA!��A!-wA ��A!%FA!E9A �cA FA�9Au%A1�A�A5�AZA	lAm]A�hAM�A��Ag�A�A	lA�A�6A�A(�A�vA6�AOvAOvAT�A�A��A��A�AZ�A��Ag8A(�A�A_pA�A�KA0�A�hA
��A
�A	�6A	�A�}A_A�A$�AںA��A�5AGEA�AiDA�KA��AxAݘA�$AC�A خA �$A /@��@��Q@�j@�	�@��`@���@��[@�$t@��@�p;@�B[@��@���@��@�i�@�@�	�@�C@�5�@��@���@�	@�h@���@�k�@�5?@�خ@��@�c@�*0@�D�@鹌@薼@��@�@��U@�u%@��@�@@��@��@��@�q@�~@�y�@��@�@��+@�}�@�p�@�}�@� \@���@޷�@�T�@ܴ9@�:�@��o@���@�bN@�3�@�7L@ؙ1@�L0@� i@�u%@�b@Ղ�@�@O@ԦL@�	@��&@���@ԃ@Ԙ_@Ԝx@ԥz@Ե@Ԋr@��@�R�@�e,@��@��?@�Ft@υ�@�B�@� \@�҉@�^5@�8�@�1@�s�@��@���@��U@�}V@�	@ˈf@���@ʵ@�l"@�@��K@�<6@Ȭ@��@�U�@��@�	l@��`@�xl@��3@�a�@�1�@Ě�@ê�@�g�@�"�@�c�@��@� \@���@�~�@�bN@�>B@��W@��*@�F@��@��r@�Xy@�:�@��-@�*0@��O@�$@���@�x@�^�@�q@��X@��@���@��{@�.I@��]@�3�@�N<@���@�S�@�@��9@�=�@���@�g8@��@��@��T@��N@�qv@�͟@��e@��F@��@�;d@�@�;@���@�Ta@���@��,@�z�@�4n@��@��@��@���@��+@�bN@�D�@��T@�?}@��|@��p@��U@��@��@���@�j@�+k@�x@���@�j@�G�@��@���@�~�@�u�@�Z�@�H@�;�@�3�@�	@�{J@�=@��`@�@�j@�7L@�@���@��@��*@��~@�n/@�*0@���@�m�@�@��-@��@�e�@�;d@���@��@�G@��~@�|@�_p@�+�@���@��]@���@�+k@�_@�@�a�@�/�@�Ĝ@�^5@��D@��-@�U�@���@�=q@��a@�Y�@��	@���@���@�kQ@�Ft@��@�˒@�rG@�`B@�P�@�=�@��M@��@�;�@��#@���@�=@�q@���@�Ov@��W@��a@��@��~@�E9@�+@���@���@�h
@�J@��a@�x�@�e�@�J�@�V@��@���@�!�@��W@��z@���@�u�@�/�@��@��K@��.@�Z�@�-�@��]@��j@��K@�_p@�V@��@���@���@�u�@�!@���@��j@���@���@�o @��"@���@�7�@�
�@���@���@���@�y�@�>�@��@��@���@�@�@�_@��M@�RT@�?}@��@��c@�n�@��D@��h@�s@�F@��@��f@��@�D�@��@���@��@�N<@�%F@��@��@���@�Ov@�$@��@�s�@��@�֡@��B@���@�M@{J@Z�@~�\@}��@|�|@|��@|��@{�[@{9�@z��@y��@x�K@x_@w�@w��@v�@v�H@v�@v�@u��@t�o@s�V@s8@r��@r��@rW�@r�@r@q��@q��@p�@p��@py>@p$@o��@oF�@n�X@n^5@m�@m�'@m}�@m\�@mA @l�)@lM@kخ@kj�@j�@jq�@j�@iԕ@i�=@is�@i2a@h�@h��@h�@g"�@f�6@f��@f&�@e��@e�H@eu�@d�9@d_@c�@c�V@c@O@b�H@b�1@b��@bs�@a��@a=�@`�f@`�p@`��@`7�@_dZ@^�"@^�@^�!@^��@^�+@^GE@]�"@\�@\M@[�@[��@[��@[;d@[�@Z��@Z($@Y�D@Y��@YN<@X�@X!@W˒@W��@W��@Wl�@W>�@W&@W�@V�b@V�@U�@U%@T�O@T��@T�@Sqv@S(@R��@R��@Ru%@RB[@R_@Q�@Q��@Qw2@Qp�@Qhs@P�	@P�_@P1'@O�@OP�@Oo@N�@N~�@ML�@L��@LQ�@L"h@K�{@KK�@J��@JYK@J
�@I�Z@I��@Iw2@I<6@I�@H��@H��@HXy@H1'@G�]@G��@F�@F� @F;�@F�@E�@E��@E��@E@D��@D2�@C�@C��@CU�@B�@B�@B��@Bl�@Bu@A��@A0�@@��@@�?@@z�@?�]@?�F@?�4@?6z@>�@>-@=�@=�=@=��@=j@=\�@=G�@=@@<�[@<�U@<�$@<��@<�4@<e�@<<�@<�@;��@;X�@:�@:kQ@:.�@9ԕ@9�@9�X@9\�@9&�@9+@9@8�)@89X@7خ@7��@7\)@7@O@6��@6�b@6C�@5�@5Vm@4�	@4�?@4�@4_@42�@4�@3��@3ƨ@3��@3��@3�@2҉@2��@2{@1��@1��@1B�@0�)@0u�@0C-@0x@/�P@/O@/@O@/9�@/&@/�@.�h@.Q@-��@-��@-c@-o @-�@,�5@,�j@,$@+��@+��@+�@*�s@*kQ@*e@)�9@)}�@)`B@)N<@)@@(j@(:�@'�]@'�@'��@'b�@'J#@'�@&�s@&�@&�L@&��@&@�@&	@%�@%�@%�9@%@%q@$�e@$��@$`�@#�Q@#�4@#C�@#�@"�h@"GE@";�@"($@"u@!��@!�@!�H@!�h@!Y�@!�@ ��@ S�@�r@��@��@qv@@�+@{�@\�@�@��@}�@�@u�@-�@�w@��@�V@U�@�@��@B[@��@�@j@S&@ \@��@�e@r�@D�@	�@�@@��@b�@��@� @u%@YK@$�@��@�C@x�@�@��@��@|�@l"@c�@tT@j@9X@�]@=@8@=@A�@�@�X@��@q�@_�@@�@0U@#:@�.@�@��@��@u�@rG@rG@s�@j@7L@4@!�@�@�@�)@�z@�_@�@g8@Ft@�@��@t�@b�@+@(@��@�,@�@�X@�@�<@��@u%@\�@M�@5?@�Z@�t@��@c�@#�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�7�A�8RA�9�A�9$A��A��A���A��(A���A��A��-A˙1A�t�A�b�A�\�A�XEA�T�A�P�A�L�A�IRA�DgA�B'A�:^A�/�A���A��AʖA�j�Aɨ�A�#nA���A��jA�ɺAȥFA�}�A�j�A�LdA���A�PHA�A��A��A���AƘ�A�1[A��AœA�A�U�A��A��A�f�A��+A�<�A�zA���A�g8A���A��A�gmA��0A��A�e�A��OA�y>A���A���A��[A�;�A��XA��A��%A�^A�A�q�A��A�ٴA�:*A��LA��vA���A�8�A���A�
�A�(�A�S�A��A���A�ɺA��A��A��A��A��A��&A�רA�{A���A�1A��4A��oA��=A���A���Ay�$As��Am($Ai�Ag�HAg6zAa�/A\|�AW��AS�ANXyAJ��AH��AF�ACS�A@tTA?�LA>hsA=>�A<��A=;�A<j�A;VmA:]dA9K^A7 �A5�A4�_A4A3��A3]�A2�+A2�rA2?A2Q�A1�\A0W?A.�fA. �A,a�A,�A,�
A,��A,�-A,C-A+�A+�A*xA)6A)D�A)JA'�qA&�,A&�A'�A&��A%&�A%�A%�DA&�"A&��A%�8A$��A$@OA#'RA!��A!-wA ��A!%FA!E9A �cA FA�9Au%A1�A�A5�AZA	lAm]A�hAM�A��Ag�A�A	lA�A�6A�A(�A�vA6�AOvAOvAT�A�A��A��A�AZ�A��Ag8A(�A�A_pA�A�KA0�A�hA
��A
�A	�6A	�A�}A_A�A$�AںA��A�5AGEA�AiDA�KA��AxAݘA�$AC�A خA �$A /@��@��Q@�j@�	�@��`@���@��[@�$t@��@�p;@�B[@��@���@��@�i�@�@�	�@�C@�5�@��@���@�	@�h@���@�k�@�5?@�خ@��@�c@�*0@�D�@鹌@薼@��@�@��U@�u%@��@�@@��@��@��@�q@�~@�y�@��@�@��+@�}�@�p�@�}�@� \@���@޷�@�T�@ܴ9@�:�@��o@���@�bN@�3�@�7L@ؙ1@�L0@� i@�u%@�b@Ղ�@�@O@ԦL@�	@��&@���@ԃ@Ԙ_@Ԝx@ԥz@Ե@Ԋr@��@�R�@�e,@��@��?@�Ft@υ�@�B�@� \@�҉@�^5@�8�@�1@�s�@��@���@��U@�}V@�	@ˈf@���@ʵ@�l"@�@��K@�<6@Ȭ@��@�U�@��@�	l@��`@�xl@��3@�a�@�1�@Ě�@ê�@�g�@�"�@�c�@��@� \@���@�~�@�bN@�>B@��W@��*@�F@��@��r@�Xy@�:�@��-@�*0@��O@�$@���@�x@�^�@�q@��X@��@���@��{@�.I@��]@�3�@�N<@���@�S�@�@��9@�=�@���@�g8@��@��@��T@��N@�qv@�͟@��e@��F@��@�;d@�@�;@���@�Ta@���@��,@�z�@�4n@��@��@��@���@��+@�bN@�D�@��T@�?}@��|@��p@��U@��@��@���@�j@�+k@�x@���@�j@�G�@��@���@�~�@�u�@�Z�@�H@�;�@�3�@�	@�{J@�=@��`@�@�j@�7L@�@���@��@��*@��~@�n/@�*0@���@�m�@�@��-@��@�e�@�;d@���@��@�G@��~@�|@�_p@�+�@���@��]@���@�+k@�_@�@�a�@�/�@�Ĝ@�^5@��D@��-@�U�@���@�=q@��a@�Y�@��	@���@���@�kQ@�Ft@��@�˒@�rG@�`B@�P�@�=�@��M@��@�;�@��#@���@�=@�q@���@�Ov@��W@��a@��@��~@�E9@�+@���@���@�h
@�J@��a@�x�@�e�@�J�@�V@��@���@�!�@��W@��z@���@�u�@�/�@��@��K@��.@�Z�@�-�@��]@��j@��K@�_p@�V@��@���@���@�u�@�!@���@��j@���@���@�o @��"@���@�7�@�
�@���@���@���@�y�@�>�@��@��@���@�@�@�_@��M@�RT@�?}@��@��c@�n�@��D@��h@�s@�F@��@��f@��@�D�@��@���@��@�N<@�%F@��@��@���@�Ov@�$@��@�s�@��@�֡@��B@���@�M@{J@Z�@~�\@}��@|�|@|��@|��@{�[@{9�@z��@y��@x�K@x_@w�@w��@v�@v�H@v�@v�@u��@t�o@s�V@s8@r��@r��@rW�@r�@r@q��@q��@p�@p��@py>@p$@o��@oF�@n�X@n^5@m�@m�'@m}�@m\�@mA @l�)@lM@kخ@kj�@j�@jq�@j�@iԕ@i�=@is�@i2a@h�@h��@h�@g"�@f�6@f��@f&�@e��@e�H@eu�@d�9@d_@c�@c�V@c@O@b�H@b�1@b��@bs�@a��@a=�@`�f@`�p@`��@`7�@_dZ@^�"@^�@^�!@^��@^�+@^GE@]�"@\�@\M@[�@[��@[��@[;d@[�@Z��@Z($@Y�D@Y��@YN<@X�@X!@W˒@W��@W��@Wl�@W>�@W&@W�@V�b@V�@U�@U%@T�O@T��@T�@Sqv@S(@R��@R��@Ru%@RB[@R_@Q�@Q��@Qw2@Qp�@Qhs@P�	@P�_@P1'@O�@OP�@Oo@N�@N~�@ML�@L��@LQ�@L"h@K�{@KK�@J��@JYK@J
�@I�Z@I��@Iw2@I<6@I�@H��@H��@HXy@H1'@G�]@G��@F�@F� @F;�@F�@E�@E��@E��@E@D��@D2�@C�@C��@CU�@B�@B�@B��@Bl�@Bu@A��@A0�@@��@@�?@@z�@?�]@?�F@?�4@?6z@>�@>-@=�@=�=@=��@=j@=\�@=G�@=@@<�[@<�U@<�$@<��@<�4@<e�@<<�@<�@;��@;X�@:�@:kQ@:.�@9ԕ@9�@9�X@9\�@9&�@9+@9@8�)@89X@7خ@7��@7\)@7@O@6��@6�b@6C�@5�@5Vm@4�	@4�?@4�@4_@42�@4�@3��@3ƨ@3��@3��@3�@2҉@2��@2{@1��@1��@1B�@0�)@0u�@0C-@0x@/�P@/O@/@O@/9�@/&@/�@.�h@.Q@-��@-��@-c@-o @-�@,�5@,�j@,$@+��@+��@+�@*�s@*kQ@*e@)�9@)}�@)`B@)N<@)@@(j@(:�@'�]@'�@'��@'b�@'J#@'�@&�s@&�@&�L@&��@&@�@&	@%�@%�@%�9@%@%q@$�e@$��@$`�@#�Q@#�4@#C�@#�@"�h@"GE@";�@"($@"u@!��@!�@!�H@!�h@!Y�@!�@ ��@ S�@�r@��@��@qv@@�+@{�@\�@�@��@}�@�@u�@-�@�w@��@�V@U�@�@��@B[@��@�@j@S&@ \@��@�e@r�@D�@	�@�@@��@b�@��@� @u%@YK@$�@��@�C@x�@�@��@��@|�@l"@c�@tT@j@9X@�]@=@8@=@A�@�@�X@��@q�@_�@@�@0U@#:@�.@�@��@��@u�@rG@rG@s�@j@7L@4@!�@�@�@�)@�z@�_@�@g8@Ft@�@��@t�@b�@+@(@��@�,@�@�X@�@�<@��@u%@\�@M�@5?@�Z@�t@��@c�@#�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
$�B
$�B
$�B
$&B
#TB
"�B
!�B
"�B
!�B
!�B
 BB
B
B
~B
dB
~B
�B
�B
�B
�B
OB
B
�B
 B
 �B
!�B
!B
�B
/B
�B
%,B
2�B
6FB
6`B
4�B
6FB
6`B
.�B
'8B
1�B
X_B
vB
�=B
��B
��B
ɠB
�2B
��B
�B�B
=B
��B
�HB�B#TB�B�B#�B^�Ba�Bf�B��B��B��B��B��B��B�DB�(B�oB֡B�B�TB�8B�(BzB9B
XB�B �B�B�B�8B�B�RBٴB҉B��B��B�B�rBq�Bf2B]�BHKB5?BB
�B
�?B
�B
��B
h�B
Y�B
3B	�B	��B	�B	��B	y$B	r�B	V�B	6�B	# B	�B	[B	B	UB�B�B��B��B�B�*B	�B	<�B	KxB	`�B	�6B	�{B	�EB	�qB	�:B	��B	ȚB	�B	�'B	�FB	�+B
�B
�B	�HB	�?B	�aB	�B	��B
�B
\B
B
!-B
!B
�B
7B
�B
jB
#nB
�B
B
# B
.IB
+B
�B
'�B
5ZB
FtB
IlB
IB
GEB
FYB
C�B
@�B
@4B
<6B
H1B
L0B
J�B
J	B
JXB
J�B
IlB
GEB
F?B
C�B
B[B
?�B
<jB
9rB
6�B
6B
5�B
0!B
# B
yB
&B
�B

�B
�B
�B
�B
[B
�B
�B
�B
 iB	��B
�B

�B
\B
NB
vB
�B
�B
oB
.B
�B
?B
?B
�B
�B	�jB	��B	��B
AB
�B
B
AB
�B
�B	��B	��B	�B	�B	�GB	�B	�MB	��B	�B	��B	� B	�8B	�B	��B	��B	�LB	��B	�B	�;B	��B	�9B	�2B	��B	��B	��B	�B	�ZB	�B	��B	�MB	�GB	��B	�B	�GB	�B	��B	�MB	�B	�B	��B	�ZB	�MB	��B	�nB	�%B	�%B	�ZB	��B	�+B	��B	�2B	�fB	��B	�LB	��B	�LB	��B	��B	��B	��B	��B	��B	�dB	�	B	��B	��B	��B	��B	��B	�9B	�aB	��B	�B	�'B	��B	�OB	��B	�B	�B	�B	�;B	�'B	�^B	��B	��B	��B	��B
 iB
�B
�B
 B
B
 �B
 �B
B
 �B
 �B
 �B
B
 �B
 �B
B
B
 �B
 �B
 �B
 �B
 �B
B
 �B
 �B
B
 �B
 �B
 �B
 �B
�B
[B
B
GB
{B
-B
�B
uB
�B
�B
B
 �B
 B
;B
�B
'B
�B
B
B
�B
�B
B
�B
'B
�B
�B
�B
�B
�B
�B
'B
'B
�B
�B
�B
uB
�B
AB
�B
'B
�B
3B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

	B

�B

�B
DB
DB
DB
xB
�B
�B
B
0B
B
�B
6B
�B
jB
jB
�B
�B
�B
�B
�B
�B
B
<B
<B
VB
�B
�B
�B
�B
�B
�B
�B
�B
vB
B
B
�B
.B
}B
bB
bB
�B
B
TB
TB
oB
&B
&B
B
�B
�B
�B
[B
@B
�B
�B
TB
TB
oB
B
�B
,B
gB
B
B
mB
mB
9B
�B
�B
?B
?B
�B
�B
�B
�B
�B
B
eB
�B
�B
kB
kB
�B
�B
�B
kB
kB
�B
�B
	B
=B
�B
B
)B
�B
/B
�B
B
5B
5B
�B
�B
!B
VB
�B
�B
 \B
 �B
 �B
 �B
 �B
!HB
!HB
!�B
"B
"4B
"NB
"�B
# B
#B
#:B
#�B
#�B
#�B
#�B
$&B
#�B
$�B
$�B
$�B
$�B
$�B
%FB
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'mB
'8B
'mB
'�B
'�B
'�B
'�B
(>B
(sB
(�B
(�B
)DB
)�B
)�B
)�B
)�B
)�B
*�B
+kB
+�B
+�B
,B
,�B
-)B
-�B
.IB
.cB
.�B
.}B
.cB
.�B
/ B
/5B
/�B
/�B
0�B
0�B
2GB
3B
33B
3B
3hB
4B
4nB
4TB
4�B
5B
5�B
5�B
5�B
6FB
6FB
6`B
6�B
6�B
7B
7fB
7�B
7�B
7�B
7�B
7fB
7fB
7�B
8�B
8�B
9XB
9�B
:*B
:DB
:DB
:*B
:B
;dB
;dB
;dB
;�B
<PB
<jB
<�B
=<B
=�B
=�B
=�B
=�B
=�B
>BB
>�B
>�B
?B
?HB
?�B
@B
@iB
@�B
@�B
@�B
@�B
@�B
A;B
BAB
B'B
BAB
B�B
B�B
B�B
B�B
CaB
C{B
C�B
C�B
C�B
DMB
DMB
D3B
D3B
D�B
ESB
E�B
E�B
E�B
E�B
FtB
F�B
F�B
F�B
F�B
F�B
F�B
GEB
G�B
HfB
H�B
HfB
H�B
H�B
IB
IRB
I�B
I�B
I�B
J#B
J�B
KB
KDB
K^B
KDB
K^B
KxB
KxB
K^B
K�B
K�B
L0B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N"B
N<B
NpB
N�B
N�B
N�B
N�B
N�B
N�B
OBB
O\B
O�B
P.B
P�B
P�B
P�B
P�B
R B
RoB
R�B
R�B
SB
R�B
S@B
S�B
S�B
S�B
TB
T,B
TFB
TaB
T�B
UB
U�B
U�B
U2B
UMB
VB
W$B
WsB
WsB
W�B
W�B
W�B
XB
X_B
XyB
XyB
XyB
XyB
X�B
X�B
X�B
X�B
YKB
YB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
[	B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\]B
\]B
\]B
\]B
\]B
\�B
\�B
\�B
]/B
]/B
]�B
]�B
^B
^�B
^�B
^jB
^�B
^�B
^�B
^�B
^�B
_pB
_�B
_�B
_�B
_�B
`'B
`\B
`�B
`�B
a-B
abB
a|B
a�B
a�B
a�B
a�B
a�B
b4B
bB
b4B
b�B
b�B
b�B
cTB
c�B
c�B
dB
dZB
d�B
d�B
d�B
eFB
e`B
e`B
eFB
e`B
e`B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
f�B
gB
gmB
g�B
g�B
h>B
hsB
h�B
iB
iDB
i�B
i�B
i�B
i�B
jeB
j�B
j�B
j�B
k6B
kQB
kQB
kkB
k�B
k�B
k�B
k�B
l=B
lqB
lqB
l�B
lqB
lqB
mCB
mwB
mwB
mCB
m�B
m�B
nIB
n/B
n�B
o5B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
pB
pUB
p�B
p�B
p�B
p�B
qvB
q[B
q�B
rB
rB
r-B
raB
r�B
r�B
shB
s�B
tB
t�B
t�B
t�B
t�B
uB
u?B
u�B
v+B
vzB
vzB
vzB
v�B
v�B
wB
wLB
wLB
wfB
w�B
w�B
w�B
xRB
y$B
y	B
y>B
y�B
z*B
z*B
zDB
zB
z^B
z�B
{0B
{B
{JB
{�B
|jB
|�B
}<B
|�B
}qB
}�B
}�B
~]B
~�B
~�B
~�B
~�B
.B
.B
.B
.B
.B
�B
}B
}B
}B
}B
cB
}B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�OB
��B
��B
��B
��B
�UB
��B
�UB
�oB
��B
��B
��B
��B
��B
�'B
�B
�AB
�AB
�uB
��B
��B
��B
��B
�B
�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
$�B
$�B
$�B
$&B
#TB
"�B
!�B
"�B
!�B
!�B
 BB
B
B
~B
dB
~B
�B
�B
�B
�B
OB
B
�B
 B
 �B
!�B
!B
�B
/B
�B
%,B
2�B
6FB
6`B
4�B
6FB
6`B
.�B
'8B
1�B
X_B
vB
�=B
��B
��B
ɠB
�2B
��B
�B�B
=B
��B
�HB�B#TB�B�B#�B^�Ba�Bf�B��B��B��B��B��B��B�DB�(B�oB֡B�B�TB�8B�(BzB9B
XB�B �B�B�B�8B�B�RBٴB҉B��B��B�B�rBq�Bf2B]�BHKB5?BB
�B
�?B
�B
��B
h�B
Y�B
3B	�B	��B	�B	��B	y$B	r�B	V�B	6�B	# B	�B	[B	B	UB�B�B��B��B�B�*B	�B	<�B	KxB	`�B	�6B	�{B	�EB	�qB	�:B	��B	ȚB	�B	�'B	�FB	�+B
�B
�B	�HB	�?B	�aB	�B	��B
�B
\B
B
!-B
!B
�B
7B
�B
jB
#nB
�B
B
# B
.IB
+B
�B
'�B
5ZB
FtB
IlB
IB
GEB
FYB
C�B
@�B
@4B
<6B
H1B
L0B
J�B
J	B
JXB
J�B
IlB
GEB
F?B
C�B
B[B
?�B
<jB
9rB
6�B
6B
5�B
0!B
# B
yB
&B
�B

�B
�B
�B
�B
[B
�B
�B
�B
 iB	��B
�B

�B
\B
NB
vB
�B
�B
oB
.B
�B
?B
?B
�B
�B	�jB	��B	��B
AB
�B
B
AB
�B
�B	��B	��B	�B	�B	�GB	�B	�MB	��B	�B	��B	� B	�8B	�B	��B	��B	�LB	��B	�B	�;B	��B	�9B	�2B	��B	��B	��B	�B	�ZB	�B	��B	�MB	�GB	��B	�B	�GB	�B	��B	�MB	�B	�B	��B	�ZB	�MB	��B	�nB	�%B	�%B	�ZB	��B	�+B	��B	�2B	�fB	��B	�LB	��B	�LB	��B	��B	��B	��B	��B	��B	�dB	�	B	��B	��B	��B	��B	��B	�9B	�aB	��B	�B	�'B	��B	�OB	��B	�B	�B	�B	�;B	�'B	�^B	��B	��B	��B	��B
 iB
�B
�B
 B
B
 �B
 �B
B
 �B
 �B
 �B
B
 �B
 �B
B
B
 �B
 �B
 �B
 �B
 �B
B
 �B
 �B
B
 �B
 �B
 �B
 �B
�B
[B
B
GB
{B
-B
�B
uB
�B
�B
B
 �B
 B
;B
�B
'B
�B
B
B
�B
�B
B
�B
'B
�B
�B
�B
�B
�B
�B
'B
'B
�B
�B
�B
uB
�B
AB
�B
'B
�B
3B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

	B

�B

�B
DB
DB
DB
xB
�B
�B
B
0B
B
�B
6B
�B
jB
jB
�B
�B
�B
�B
�B
�B
B
<B
<B
VB
�B
�B
�B
�B
�B
�B
�B
�B
vB
B
B
�B
.B
}B
bB
bB
�B
B
TB
TB
oB
&B
&B
B
�B
�B
�B
[B
@B
�B
�B
TB
TB
oB
B
�B
,B
gB
B
B
mB
mB
9B
�B
�B
?B
?B
�B
�B
�B
�B
�B
B
eB
�B
�B
kB
kB
�B
�B
�B
kB
kB
�B
�B
	B
=B
�B
B
)B
�B
/B
�B
B
5B
5B
�B
�B
!B
VB
�B
�B
 \B
 �B
 �B
 �B
 �B
!HB
!HB
!�B
"B
"4B
"NB
"�B
# B
#B
#:B
#�B
#�B
#�B
#�B
$&B
#�B
$�B
$�B
$�B
$�B
$�B
%FB
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'mB
'8B
'mB
'�B
'�B
'�B
'�B
(>B
(sB
(�B
(�B
)DB
)�B
)�B
)�B
)�B
)�B
*�B
+kB
+�B
+�B
,B
,�B
-)B
-�B
.IB
.cB
.�B
.}B
.cB
.�B
/ B
/5B
/�B
/�B
0�B
0�B
2GB
3B
33B
3B
3hB
4B
4nB
4TB
4�B
5B
5�B
5�B
5�B
6FB
6FB
6`B
6�B
6�B
7B
7fB
7�B
7�B
7�B
7�B
7fB
7fB
7�B
8�B
8�B
9XB
9�B
:*B
:DB
:DB
:*B
:B
;dB
;dB
;dB
;�B
<PB
<jB
<�B
=<B
=�B
=�B
=�B
=�B
=�B
>BB
>�B
>�B
?B
?HB
?�B
@B
@iB
@�B
@�B
@�B
@�B
@�B
A;B
BAB
B'B
BAB
B�B
B�B
B�B
B�B
CaB
C{B
C�B
C�B
C�B
DMB
DMB
D3B
D3B
D�B
ESB
E�B
E�B
E�B
E�B
FtB
F�B
F�B
F�B
F�B
F�B
F�B
GEB
G�B
HfB
H�B
HfB
H�B
H�B
IB
IRB
I�B
I�B
I�B
J#B
J�B
KB
KDB
K^B
KDB
K^B
KxB
KxB
K^B
K�B
K�B
L0B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N"B
N<B
NpB
N�B
N�B
N�B
N�B
N�B
N�B
OBB
O\B
O�B
P.B
P�B
P�B
P�B
P�B
R B
RoB
R�B
R�B
SB
R�B
S@B
S�B
S�B
S�B
TB
T,B
TFB
TaB
T�B
UB
U�B
U�B
U2B
UMB
VB
W$B
WsB
WsB
W�B
W�B
W�B
XB
X_B
XyB
XyB
XyB
XyB
X�B
X�B
X�B
X�B
YKB
YB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
[	B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\]B
\]B
\]B
\]B
\]B
\�B
\�B
\�B
]/B
]/B
]�B
]�B
^B
^�B
^�B
^jB
^�B
^�B
^�B
^�B
^�B
_pB
_�B
_�B
_�B
_�B
`'B
`\B
`�B
`�B
a-B
abB
a|B
a�B
a�B
a�B
a�B
a�B
b4B
bB
b4B
b�B
b�B
b�B
cTB
c�B
c�B
dB
dZB
d�B
d�B
d�B
eFB
e`B
e`B
eFB
e`B
e`B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
f�B
gB
gmB
g�B
g�B
h>B
hsB
h�B
iB
iDB
i�B
i�B
i�B
i�B
jeB
j�B
j�B
j�B
k6B
kQB
kQB
kkB
k�B
k�B
k�B
k�B
l=B
lqB
lqB
l�B
lqB
lqB
mCB
mwB
mwB
mCB
m�B
m�B
nIB
n/B
n�B
o5B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
pB
pUB
p�B
p�B
p�B
p�B
qvB
q[B
q�B
rB
rB
r-B
raB
r�B
r�B
shB
s�B
tB
t�B
t�B
t�B
t�B
uB
u?B
u�B
v+B
vzB
vzB
vzB
v�B
v�B
wB
wLB
wLB
wfB
w�B
w�B
w�B
xRB
y$B
y	B
y>B
y�B
z*B
z*B
zDB
zB
z^B
z�B
{0B
{B
{JB
{�B
|jB
|�B
}<B
|�B
}qB
}�B
}�B
~]B
~�B
~�B
~�B
~�B
.B
.B
.B
.B
.B
�B
}B
}B
}B
}B
cB
}B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�OB
��B
��B
��B
��B
�UB
��B
�UB
�oB
��B
��B
��B
��B
��B
�'B
�B
�AB
�AB
�uB
��B
��B
��B
��B
�B
�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105245  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192454  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192454  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192454                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042501  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042501  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                