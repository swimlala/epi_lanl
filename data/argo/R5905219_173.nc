CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-25T21:41:55Z creation;2022-12-25T21:41:57Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20221225214155  20221225215824  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�(�7�I1   @�+�W;@4`     �c�KƧ�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Ck�fCn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D fD �fD!  D!� D"  D"� D#  D#y�D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp�fDqfDq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�C3Dƃ3D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D���D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�D�|�D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@`��@��@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B

=B
=B
=B!��B*
=B2
=B:
=BB
=BJ
=BR
=BZ
=Bbp�Bj
=Br
=Bzp�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C h�C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CHh�CJh�CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd�)Cf��Ch��Cj��Clh�Cn��Cp��Cr��Cth�Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�4{C�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�4{C�4{C�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D'
D�
D �D��D �D��D �D��D �D��D �D�
D �D��D �D��D �D��D '
D �
D! �D!��D" �D"��D# �D#�>D$>D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8>D8��D9 �D9��D: �D:�>D; �D;��D< �D<��D= �D=��D> �D>��D?>D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD>DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL�
DM �DM��DN �DN��DO'
DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df'
Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm'
Dm��Dn �Dn��Do �Do��Dp �Dp�
Dq'
Dq�
Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�MD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��D�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��D��D�RD�S�D��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD��D�PRD��RD��RD�RD�PRD���D��RD�RD�PRD��RD��RD�RD�PRD��RD��D�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�D�PRD��RD��RD�RD�PRD��RD�ӅD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��D��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�MD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD��D�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�S�D���D��RD�RD�PRDRD��RD�RD�PRDÐRD��RD�RD�PRDĐRD��RD�RD�PRDŐRD��RD�RD�S�DƓ�D��RD�RD�PRDǐRD��RD��D�PRDȐRD��RD�RD�PRDɐRD��RD�RD�PRDʐRD��RD�RD�PRDːRD��RD�RD�PRD̐RD��RD�RD�PRD͐RD��RD�D�PRDΐRD��RD�RD�PRDϐRD��RD�RD�PRDАRD��RD�RD�PRDѐRD��RD�RD�PRDҐRD��RD�RD�PRDӐRD��RD�RD�PRDԐRD��RD�RD�PRDՐRD��RD�RD�PRD֐RD��RD�RD�PRDאRD��RD�RD�PRDؐRD��RD�RD�PRDِRD��RD�D�MDڍD��RD�RD�PRDېRD��RD�RD�PRDܐRD��RD�RD�PRDݐRD��RD�RD�PRDސRD��RD�RD�PRDߐRD��RD�RD�PRD��RD��D�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��D�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD�ӅD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�MD�RD��RD�RD�MD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�D�PRD��RD�ÅD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��
A���A���A���A���A���A���A���A���A���A��;A��TA��TA��;A��TA��A�  A�+A�ffA�|�Aω7A�r�A�n�A�VA�A�A�-A�&�A��A�oA���AΑhA�z�A�O�A�/A��A�A���A͕�A�C�A��A̼jA��A�C�A�5?A�ĜA�G�A���A�\)A�\)A�JA��A��PA��FA�$�A�p�A��wA�1'A���A��jA���A��+A�ZA���A���A���A�1A��A�=qA��/A��DA��\A�&�A��
A�  A�bA�ĜA�A��\A�^5A��A�ffA�l�A��#A�VA���A�9XA�l�A��jA��A�?}A��7A��HA��A�Q�A��FA�p�A�hsA�`BA��;A�l�A�?}A���A�hsA�K�A�ȴA�bA�  A���A��`A�t�A���A��+A��yA�9XAz-AwVAr�HAk�mAg�PAc��A_��A\=qAX9XAV�`ATVARQ�AP�ANA�ALbNAJ  AG��AD��AB�AB�DAA��A@��A?ƨA>^5A<9XA:v�A8E�A6E�A4n�A2��A1��A/\)A,�A*�\A(I�A&��A%�A#&�A!��A!\)A!�A �A�
A�A�A�!A�A�A�AVAQ�A5?A�
A?}A��AĜA+A�jA�uA�A
�A	�A	hsA��Ax�A
=A�DA��Al�A�AA��A��AbNA��A?}A�jAz�A��A ��A b@�dZ@�ff@�7L@��`@���@��9@���@��@��!@�V@���@�&�@���@�C�@�hs@�A�@�^5@�-@�p�@��@�l�@���@��@��@��@�Q�@�E�@��@� �@�t�@�o@ޗ�@��@�O�@�&�@��@�V@��@�j@�t�@�ȴ@�{@�`B@�A�@׍P@���@��@��@�;d@�5?@�@ѩ�@ϕ�@�E�@�x�@̴9@�(�@��m@˶F@�+@��@�M�@���@���@Ȭ@ȃ@���@�S�@��@��y@ƸR@��y@�|�@�K�@��@�5?@�?}@ċD@î@�o@���@�@�ff@��@�@��@��@�I�@���@��R@�V@��7@�/@���@���@��@���@�l�@���@�~�@���@���@�V@��/@��u@�r�@� �@���@��@�+@���@��\@�V@��@���@�X@���@���@��D@�z�@�Q�@��;@�t�@�+@���@���@�-@��h@�p�@�?}@��@��@��@�l�@��@�5?@���@�7L@���@�j@�(�@�l�@��@���@�J@�@�p�@�/@��u@�bN@�1'@�b@��@���@�+@���@�~�@�=q@�@��#@���@��@�X@�7L@�%@��D@�Q�@�I�@�I�@�(�@�1@��@�C�@��H@���@�E�@�J@���@��-@���@�p�@�X@�V@���@��j@�Z@�1@��
@��w@��@��P@�K�@�+@��y@���@��@���@��u@�r�@�Z@�9X@�(�@�1@��;@��w@���@�|�@�t�@�dZ@�S�@�33@�+@��@��y@�E�@�$�@�@��T@���@���@�`B@�V@�Ĝ@��u@��@�j@�A�@�b@�  @���@��
@�|�@��@���@��@�ȴ@��R@���@���@���@��\@�n�@�-@��@��-@�hs@��@��@���@���@�Q�@�1'@��@�b@�  @��
@��P@�dZ@�;d@�o@��H@���@��\@�^5@��h@�p�@�`B@�G�@��j@�j@�9X@�(�@� �@��;@�K�@�C�@��@�@��H@���@�M�@���@��7@�hs@�&�@��`@��D@�Z@��@�ƨ@��F@��@���@���@�t�@�S�@�"�@�o@��y@���@�~�@�V@�-@�J@��T@���@���@��h@�X@�/@��@���@�bN@�bN@��@��
@��F@�dZ@�C�@�;d@�"�@��H@���@���@�M�@��@��@���@��@�X@�/@�V@��@��D@�r�@�(�@� �@��@��F@���@��P@�dZ@�;d@�+@��H@��!@�ff@�-@��T@���@�V@��/@���@|�@~��@~��@~v�@~ff@~E�@}�@}O�@{��@{�
@{��@z�H@z~�@z-@y�@y�^@y�7@y��@yx�@y&�@x�`@w�;@w\)@w
=@v�+@v5?@v5?@v$�@u�@u��@u��@u��@u�@u?}@t�D@s�@sC�@s33@s33@q�#@q��@qX@q�@p�9@oK�@o�@n�y@n5?@l�@kƨ@kdZ@j�@j�!@j^5@jJ@i�7@h��@h�u@hr�@hbN@hQ�@hA�@g�w@g�@g\)@f�R@e�-@d��@dI�@d9X@d(�@d1@cƨ@cC�@b�H@bM�@a�#@a��@a&�@`Ĝ@`Q�@_�P@_
=@^E�@]�@]�T@]��@]��@]��@]p�@]`B@\��@\z�@[�
@[S�@Z��@Z-@Y�^@YX@X��@XbN@W�P@W+@V��@Vȴ@Vv�@V$�@UO�@T��@T�D@Tz�@S�@R�@R~�@R^5@RM�@R�@Q��@Q7L@PĜ@PA�@O;d@Nff@M�-@M?}@MV@L�D@K��@K�@KC�@J�H@J��@JM�@J�@I�@Ihs@IG�@I&�@I%@H�9@H �@G�;@G�;@G��@G�w@GK�@Fȴ@Fff@F5?@F$�@E�@E`B@E?}@E�@E�@EV@D��@D��@Dj@C��@Ct�@C@B��@Bn�@B=q@B-@BJ@A�@A��@A�7@Ax�@@��@@��@@r�@@b@?��@?\)@?
=@>ȴ@>�+@>E�@>@=�@=�T@=��@=�-@=�@=?}@<�/@<��@<(�@;�F@;S�@:�H@:M�@9��@9�#@9��@9�^@9��@9�7@9hs@9X@9&�@8�9@8bN@7�w@7�@6��@6$�@5O�@4Z@3�m@3dZ@2�@2��@2~�@2^5@2-@1x�@1X@0�`@0Ĝ@0bN@01'@0b@/�P@/l�@/;d@/+@/�@/�@/
=@.�y@.ȴ@.ff@.ff@.E�@-@-��@-��@-�h@-`B@-/@-/@-�@,��@,�j@,I�@+�
@+��@+�@+S�@+"�@*�@*�@+@+@*�@*��@*�\@*n�@*=q@)��@)��@)�7@)x�@)X@)7L@(��@(Q�@(  @'�@'�;@'��@'�P@'|�@'l�@'\)@';d@'�@&ȴ@&��@&v�@&$�@%��@%p�@$�@$�j@$�D@$(�@#ƨ@#t�@#S�@#"�@#@#@"�@"��@"~�@"-@!�@!&�@  �@�w@�@�@�@|�@�@��@��@��@��@��@ȴ@E�@��@@��@�h@�@�@`B@?}@/@V@�/@�@z�@�F@@��@��@�\@n�@M�@=q@-@��@�#@��@hs@&�@�`@�@A�@ �@  @�@�@�;@��@+@�y@�@�R@�R@��@5?@�T@�h@?}@V@��@��@�@�D@9X@�@��@ƨ@�F@�@�@�@dZ@"�@�@�!@n�@=q@-@-@-@-@=q@J@�#@��@x�@X@%@��@Ĝ@�u@�@Q�@A�@1'@1'@1'@b@��@�P@\)@;d@�y@ȴ@��@v�@�T@��@��@��@�h@`B@/@�/@�D@(�@1@�m@�F@�@S�@"�@"�@"�@@
��@
~�@
^5@
^5@
=q@	�#@	�7@	hs@	7L@	7L@	&�@	&�@	�@��@�`@��@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��
A���A���A���A���A���A���A���A���A���A��;A��TA��TA��;A��TA��A�  A�+A�ffA�|�Aω7A�r�A�n�A�VA�A�A�-A�&�A��A�oA���AΑhA�z�A�O�A�/A��A�A���A͕�A�C�A��A̼jA��A�C�A�5?A�ĜA�G�A���A�\)A�\)A�JA��A��PA��FA�$�A�p�A��wA�1'A���A��jA���A��+A�ZA���A���A���A�1A��A�=qA��/A��DA��\A�&�A��
A�  A�bA�ĜA�A��\A�^5A��A�ffA�l�A��#A�VA���A�9XA�l�A��jA��A�?}A��7A��HA��A�Q�A��FA�p�A�hsA�`BA��;A�l�A�?}A���A�hsA�K�A�ȴA�bA�  A���A��`A�t�A���A��+A��yA�9XAz-AwVAr�HAk�mAg�PAc��A_��A\=qAX9XAV�`ATVARQ�AP�ANA�ALbNAJ  AG��AD��AB�AB�DAA��A@��A?ƨA>^5A<9XA:v�A8E�A6E�A4n�A2��A1��A/\)A,�A*�\A(I�A&��A%�A#&�A!��A!\)A!�A �A�
A�A�A�!A�A�A�AVAQ�A5?A�
A?}A��AĜA+A�jA�uA�A
�A	�A	hsA��Ax�A
=A�DA��Al�A�AA��A��AbNA��A?}A�jAz�A��A ��A b@�dZ@�ff@�7L@��`@���@��9@���@��@��!@�V@���@�&�@���@�C�@�hs@�A�@�^5@�-@�p�@��@�l�@���@��@��@��@�Q�@�E�@��@� �@�t�@�o@ޗ�@��@�O�@�&�@��@�V@��@�j@�t�@�ȴ@�{@�`B@�A�@׍P@���@��@��@�;d@�5?@�@ѩ�@ϕ�@�E�@�x�@̴9@�(�@��m@˶F@�+@��@�M�@���@���@Ȭ@ȃ@���@�S�@��@��y@ƸR@��y@�|�@�K�@��@�5?@�?}@ċD@î@�o@���@�@�ff@��@�@��@��@�I�@���@��R@�V@��7@�/@���@���@��@���@�l�@���@�~�@���@���@�V@��/@��u@�r�@� �@���@��@�+@���@��\@�V@��@���@�X@���@���@��D@�z�@�Q�@��;@�t�@�+@���@���@�-@��h@�p�@�?}@��@��@��@�l�@��@�5?@���@�7L@���@�j@�(�@�l�@��@���@�J@�@�p�@�/@��u@�bN@�1'@�b@��@���@�+@���@�~�@�=q@�@��#@���@��@�X@�7L@�%@��D@�Q�@�I�@�I�@�(�@�1@��@�C�@��H@���@�E�@�J@���@��-@���@�p�@�X@�V@���@��j@�Z@�1@��
@��w@��@��P@�K�@�+@��y@���@��@���@��u@�r�@�Z@�9X@�(�@�1@��;@��w@���@�|�@�t�@�dZ@�S�@�33@�+@��@��y@�E�@�$�@�@��T@���@���@�`B@�V@�Ĝ@��u@��@�j@�A�@�b@�  @���@��
@�|�@��@���@��@�ȴ@��R@���@���@���@��\@�n�@�-@��@��-@�hs@��@��@���@���@�Q�@�1'@��@�b@�  @��
@��P@�dZ@�;d@�o@��H@���@��\@�^5@��h@�p�@�`B@�G�@��j@�j@�9X@�(�@� �@��;@�K�@�C�@��@�@��H@���@�M�@���@��7@�hs@�&�@��`@��D@�Z@��@�ƨ@��F@��@���@���@�t�@�S�@�"�@�o@��y@���@�~�@�V@�-@�J@��T@���@���@��h@�X@�/@��@���@�bN@�bN@��@��
@��F@�dZ@�C�@�;d@�"�@��H@���@���@�M�@��@��@���@��@�X@�/@�V@��@��D@�r�@�(�@� �@��@��F@���@��P@�dZ@�;d@�+@��H@��!@�ff@�-@��T@���@�V@��/@���@|�@~��@~��@~v�@~ff@~E�@}�@}O�@{��@{�
@{��@z�H@z~�@z-@y�@y�^@y�7@y��@yx�@y&�@x�`@w�;@w\)@w
=@v�+@v5?@v5?@v$�@u�@u��@u��@u��@u�@u?}@t�D@s�@sC�@s33@s33@q�#@q��@qX@q�@p�9@oK�@o�@n�y@n5?@l�@kƨ@kdZ@j�@j�!@j^5@jJ@i�7@h��@h�u@hr�@hbN@hQ�@hA�@g�w@g�@g\)@f�R@e�-@d��@dI�@d9X@d(�@d1@cƨ@cC�@b�H@bM�@a�#@a��@a&�@`Ĝ@`Q�@_�P@_
=@^E�@]�@]�T@]��@]��@]��@]p�@]`B@\��@\z�@[�
@[S�@Z��@Z-@Y�^@YX@X��@XbN@W�P@W+@V��@Vȴ@Vv�@V$�@UO�@T��@T�D@Tz�@S�@R�@R~�@R^5@RM�@R�@Q��@Q7L@PĜ@PA�@O;d@Nff@M�-@M?}@MV@L�D@K��@K�@KC�@J�H@J��@JM�@J�@I�@Ihs@IG�@I&�@I%@H�9@H �@G�;@G�;@G��@G�w@GK�@Fȴ@Fff@F5?@F$�@E�@E`B@E?}@E�@E�@EV@D��@D��@Dj@C��@Ct�@C@B��@Bn�@B=q@B-@BJ@A�@A��@A�7@Ax�@@��@@��@@r�@@b@?��@?\)@?
=@>ȴ@>�+@>E�@>@=�@=�T@=��@=�-@=�@=?}@<�/@<��@<(�@;�F@;S�@:�H@:M�@9��@9�#@9��@9�^@9��@9�7@9hs@9X@9&�@8�9@8bN@7�w@7�@6��@6$�@5O�@4Z@3�m@3dZ@2�@2��@2~�@2^5@2-@1x�@1X@0�`@0Ĝ@0bN@01'@0b@/�P@/l�@/;d@/+@/�@/�@/
=@.�y@.ȴ@.ff@.ff@.E�@-@-��@-��@-�h@-`B@-/@-/@-�@,��@,�j@,I�@+�
@+��@+�@+S�@+"�@*�@*�@+@+@*�@*��@*�\@*n�@*=q@)��@)��@)�7@)x�@)X@)7L@(��@(Q�@(  @'�@'�;@'��@'�P@'|�@'l�@'\)@';d@'�@&ȴ@&��@&v�@&$�@%��@%p�@$�@$�j@$�D@$(�@#ƨ@#t�@#S�@#"�@#@#@"�@"��@"~�@"-@!�@!&�@  �@�w@�@�@�@|�@�@��@��@��@��@��@ȴ@E�@��@@��@�h@�@�@`B@?}@/@V@�/@�@z�@�F@@��@��@�\@n�@M�@=q@-@��@�#@��@hs@&�@�`@�@A�@ �@  @�@�@�;@��@+@�y@�@�R@�R@��@5?@�T@�h@?}@V@��@��@�@�D@9X@�@��@ƨ@�F@�@�@�@dZ@"�@�@�!@n�@=q@-@-@-@-@=q@J@�#@��@x�@X@%@��@Ĝ@�u@�@Q�@A�@1'@1'@1'@b@��@�P@\)@;d@�y@ȴ@��@v�@�T@��@��@��@�h@`B@/@�/@�D@(�@1@�m@�F@�@S�@"�@"�@"�@@
��@
~�@
^5@
^5@
=q@	�#@	�7@	hs@	7L@	7L@	&�@	&�@	�@��@�`@��@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
=BDBDBDBDBDBDBDBDBDBbB�B�B�B�B �B%�B-B=qB]/Bo�By�B�B�1B�=B�JB�PB�PB�VB�\B��B��B��B��B��B��B��B��B��B�B�B��B��B��B�bB�DB�7B�+B�1B��B��B��BƨBȴBɺB��B��BɺBȴBƨBƨBŢBB�jB�'B��B��B��B�{B�JB�JB�7B�1B�B�Bt�Bp�BiyBYBA�B8RB5?B&�B�BB�B�sB�BB�B�#B�B�BɺB�jB�B��B�bBs�BjBP�B:^B$�B{B+BBB
�mB
��B
ŢB
��B
��B
�JB
�B
m�B
R�B
(�B
VB	��B	ƨB	�B	�uB	}�B	m�B	W
B	K�B	@�B	2-B	$�B	�B	VB	B��B�B�ZB��B�B�B�yB�TB�#B��B��BɺB��BĜB�}B�-B��B��B��B�DB�=B�VB�=B�JB�VB�\B�PB�1B�B�+B�=B�7B�7B�bB�hB�hB��B��B��B��B�JB�1B�Bx�B~�B~�B�B�B�B�1B�7B�+B�+B�%B�%B�%B�+B�+B�+B�B�%B�1B�JB�\B�\B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�'B�RB�jBŢBǮBȴBȴBɺBɺBɺBɺBɺBɺBɺBȴBɺB��B��B��B��B��B��B��B�
B�B�;B�BB�NB�ZB�B�B�B�B�B�B�B��B��B	B	B	B	1B		7B	
=B	PB	bB	hB	uB	�B	�B	 �B	 �B	#�B	%�B	'�B	.B	1'B	2-B	33B	49B	5?B	6FB	7LB	9XB	=qB	>wB	>wB	?}B	F�B	F�B	H�B	J�B	N�B	N�B	O�B	Q�B	S�B	W
B	ZB	`BB	aHB	bNB	cTB	cTB	e`B	hsB	n�B	p�B	t�B	v�B	w�B	x�B	z�B	{�B	|�B	|�B	}�B	}�B	� B	� B	�B	� B	�B	�B	�B	�%B	�%B	�=B	�VB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�?B	�LB	�XB	�XB	�^B	�dB	�jB	�wB	��B	��B	��B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
DB
JB
PB
PB
VB
VB
\B
bB
bB
bB
bB
hB
hB
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
+B
+B
,B
,B
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
.B
.B
/B
/B
0!B
0!B
0!B
0!B
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
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
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
:^B
;dB
;dB
;dB
;dB
=qB
<jB
=qB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
T�B
W
B
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
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
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
dZB
dZB
e`B
e`B
ffB
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
m�B
m�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
p�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
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
x�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
}�B
~�B
~�B
~�B
~�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
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
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
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
�PB
�VB
�VB
�\B
�\B
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
�hB
�oB
�oB
�oB
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
�uB
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
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333B
=BDBDBDBDBDBDBDBDBDBbB�B�B�B�B �B%�B-B=qB]/Bo�By�B�B�1B�=B�JB�PB�PB�VB�\B��B��B��B��B��B��B��B��B��B�B�B��B��B��B�bB�DB�7B�+B�1B��B��B��BƨBȴBɺB��B��BɺBȴBƨBƨBŢBB�jB�'B��B��B��B�{B�JB�JB�7B�1B�B�Bt�Bp�BiyBYBA�B8RB5?B&�B�BB�B�sB�BB�B�#B�B�BɺB�jB�B��B�bBs�BjBP�B:^B$�B{B+BBB
�mB
��B
ŢB
��B
��B
�JB
�B
m�B
R�B
(�B
VB	��B	ƨB	�B	�uB	}�B	m�B	W
B	K�B	@�B	2-B	$�B	�B	VB	B��B�B�ZB��B�B�B�yB�TB�#B��B��BɺB��BĜB�}B�-B��B��B��B�DB�=B�VB�=B�JB�VB�\B�PB�1B�B�+B�=B�7B�7B�bB�hB�hB��B��B��B��B�JB�1B�Bx�B~�B~�B�B�B�B�1B�7B�+B�+B�%B�%B�%B�+B�+B�+B�B�%B�1B�JB�\B�\B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�'B�RB�jBŢBǮBȴBȴBɺBɺBɺBɺBɺBɺBɺBȴBɺB��B��B��B��B��B��B��B�
B�B�;B�BB�NB�ZB�B�B�B�B�B�B�B��B��B	B	B	B	1B		7B	
=B	PB	bB	hB	uB	�B	�B	 �B	 �B	#�B	%�B	'�B	.B	1'B	2-B	33B	49B	5?B	6FB	7LB	9XB	=qB	>wB	>wB	?}B	F�B	F�B	H�B	J�B	N�B	N�B	O�B	Q�B	S�B	W
B	ZB	`BB	aHB	bNB	cTB	cTB	e`B	hsB	n�B	p�B	t�B	v�B	w�B	x�B	z�B	{�B	|�B	|�B	}�B	}�B	� B	� B	�B	� B	�B	�B	�B	�%B	�%B	�=B	�VB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�?B	�LB	�XB	�XB	�^B	�dB	�jB	�wB	��B	��B	��B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
DB
JB
PB
PB
VB
VB
\B
bB
bB
bB
bB
hB
hB
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
+B
+B
,B
,B
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
.B
.B
/B
/B
0!B
0!B
0!B
0!B
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
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
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
:^B
;dB
;dB
;dB
;dB
=qB
<jB
=qB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
T�B
W
B
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
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
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
dZB
dZB
e`B
e`B
ffB
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
m�B
m�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
p�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
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
x�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
}�B
~�B
~�B
~�B
~�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
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
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
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
�PB
�VB
�VB
�\B
�\B
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
�hB
�oB
�oB
�oB
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
�uB
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
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20221226064135  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221225214155  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221225214156  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221225214157                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221225214157  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221225214157  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221225215824                      G�O�G�O�G�O�                