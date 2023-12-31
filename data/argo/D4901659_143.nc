CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-06-29T17:02:36Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\        �\Argo profile    3.1 1.2 19500101000000  20180629170236  20230721230917  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�n���)1   @�nq�)p@;�-�c�I�^51   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @&ff@�  @�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A���A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  Dy�D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@!G�@z�H@�p�@�p�A�A=�A^�RA~�RA�\)A�\)A�\)A�\)A�\)A�(�A�\A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B��
B��
B��
B�
=B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�DwGDwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dt{D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�:>D�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��HA��;A��yA��A��A��A��A��A���A���A��A���A��A���A�/APA�;dA�G�A�%A�(�A��/A�{A�O�A��hA�bA�~�A��wA���A��uA�ffA��9A��+A��/A�t�A���A�/A��A�%A��mA�;dA���A�A�A��wA�jA��A��TA�&�A��PA��PA��A�bA� �A���A��uA�K�A�-A�VA��A���A�~�A���A���A���A��
A�l�A�VA���A�~�A��A�33A�5?A���A���A��DA�ffA�A�p�A��A��A���A�"�A|��A{�^A{
=AzAx�Aw�Avr�At�As+AqƨAp�+AohsAn��Am��Al��Ak��Aj�9Ait�AhVAf�yAf1Ae�Ad��AcoAbI�Aa7LA^��A^E�A]�wA]��A]�A]A[XAY�hAXQ�AW�hAV��AU%ATbNASAQ��AQoAP��AP(�AOK�AN��AM�;AM�ALjAK�AJ�AI��AIVAH�AHJAG+AF��AEƨAEK�ADI�AC�-AC��ACC�AB��ABv�ABJA@5?A>��A=33A<�9A;�mA;XA:�yA:  A9l�A8�\A7�-A6��A6  A5��A5oA4�9A4��A3�TA2�A2z�A25?A1��A0ffA.�+A.(�A-�-A-7LA,�!A,1'A+O�A*$�A)C�A(ZA'��A&��A&$�A%�A$bA!VA bA�#A�A�A��An�A�#A?}AM�A�#A�Az�A9XA�
AXA�/AbNA��A"�AZAl�A��A��A �A�yA�;Az�A
r�A
5?A
$�A	�TA	��A	��A	7LAVA=qAbA��A33An�A��AK�AƨAS�A �+A -@��@�C�@��@��!@�5?@��7@�  @���@��@�bN@�t�@�ff@���@�;d@�@� �@@�X@��`@�1@�^5@�p�@蛦@��@�1@�\)@旍@�h@�r�@���@�x�@��@�K�@ް!@�@�/@���@�M�@ّh@�/@�I�@֧�@���@�I�@�=q@�hs@� �@�l�@��@θR@�M�@̓u@�l�@ʏ\@�@��@��;@�E�@���@�9X@�C�@¸R@�hs@���@�"�@�J@��-@��9@�\)@��R@�?}@�1@��+@��#@��h@�G�@��`@� �@�l�@�ȴ@�X@�/@��@��@�%@���@�dZ@�33@���@��/@�1'@��F@�t�@�\)@�"�@�
=@��y@��!@��\@�v�@�=q@��@�@���@�x�@�&�@���@��@���@���@�$�@��h@�7L@�&�@��@��j@��u@�Q�@��;@�t�@���@��-@�bN@��F@�@�
=@�n�@���@�hs@�Ĝ@�(�@�t�@�C�@�"�@��H@�E�@��T@��^@�`B@��@�K�@���@��@�v�@�J@���@��-@�x�@���@�bN@���@�C�@��!@���@��+@��@���@�Q�@��@�ƨ@���@�|�@��R@�^5@�$�@�$�@�{@�J@���@��@��/@���@� �@���@�ƨ@�\)@�33@�^5@��^@�%@�A�@��
@���@���@���@��@�l�@�K�@�@���@��@�Q�@��@��@��;@��
@��
@�dZ@��y@��@���@�=q@�n�@��@��7@�/@�/@�&�@���@��`@��@� �@��@�P@|�@l�@\)@
=@~v�@~ff@~{@}?}@|I�@{�
@{��@{dZ@{"�@zM�@y�^@y��@zJ@z�@|I�@|��@y�@x�`@xr�@x�u@w�;@v��@u�T@v$�@u��@up�@uO�@u�@u@v@u��@tz�@r��@rJ@q&�@q%@p�`@p��@pĜ@p��@p�@p1'@o�@ol�@oK�@o+@n�y@nff@m�-@m�@l��@lZ@l9X@k�F@kC�@k33@k@j�H@j��@jn�@j-@jJ@i�@iX@h��@hĜ@hĜ@h��@h�u@h�@hr�@h1'@gK�@f�+@fE�@e�@d��@d9X@d�@c��@c�F@c��@c33@b��@b�\@b^5@b�@a�@a�#@a��@a�^@a�7@aX@a%@`��@`��@`�`@a�@`��@`�9@`A�@`1'@_�;@_\)@_+@_
=@^ȴ@^��@^�+@^V@^$�@]�@]�@]O�@\��@\�/@\�@\j@[�m@[t�@["�@Z��@Z=q@Y�7@YX@X�9@Xb@W�@Xb@X1'@X  @W�;@W�w@W�@W�P@Wl�@W\)@Wl�@WK�@WK�@W�@V�@V��@V�+@VE�@V@U��@U�-@U�@T�@T�@Sƨ@S��@S��@S�F@S��@St�@St�@SC�@S@R�H@R�!@R=q@Q��@QG�@PĜ@PQ�@P  @O�;@O�;@O��@O��@O|�@O+@N��@N@Mp�@MO�@M/@L��@L��@L�@L��@LI�@L(�@L�@L�@K��@K33@K33@KC�@KC�@K@J��@J�!@J��@Jn�@I��@IG�@I%@HĜ@H�9@Hb@GK�@GK�@G;d@G;d@G;d@G+@G+@G�@F�y@F�+@FE�@F@EO�@EV@D�@D�@D1@C�@C"�@B��@B-@A��@A�@A�#@A�^@A��@Ax�@AG�@@�9@@ �@?�@?�w@?\)@>�@>�R@>�+@>$�@=�T@=�@<�/@;��@;t�@;C�@;"�@:�H@:�\@:=q@9X@9�@8�9@8�@8bN@8 �@7�@7�w@7�w@7�@7��@7|�@7K�@7K�@7;d@7
=@6�+@5@5�-@5�@4�@3�F@3S�@333@3o@2�@2��@2^5@1�@1�7@1X@0��@0Ĝ@0bN@/�@/\)@/K�@/�@.�y@.��@.5?@.@-�@-��@-�@-O�@,�@,��@,�@,��@,�D@,z�@,Z@+�
@+�F@+�@+33@+o@+@*�H@*�H@*^5@*J@)��@(�`@(Ĝ@(��@(�u@(r�@'�;@'�@'�P@'K�@';d@'+@&��@&��@&ff@&V@&E�@&5?@&$�@%�@%@%�h@%p�@%`B@%/@$�@$Z@$9X@$(�@$1@#�F@#��@#o@"-@!�#@!��@!X@!7L@!&�@!%@ ��@ �u@ A�@ b@��@+@�@�@�@�@
=@
=@��@�y@ff@E�@5?@$�@��@�-@��@�@?}@V@��@�D@1@�F@��@��@��@��@��@�@dZ@dZ@S�@S�@S�@C�@@��@��@��@�\@~�@n�@^5@J@�@��@�^@x�@�@��@�9@�u@�@r�@Q�@ �@b@�w@�P@\)@K�@
=@�y@�R@��@v�@v�@V@5?@{@�@��@�@?}@�@Z@(�@�@��@��@dZ@"�@�H@��@��@��@M�@J@�@��@X@�@�`@�u@�@�@r�@r�@r�@Q�@A�@1'@ �@��@
=@E�@��@@�-@�-@��@�h@p�@�@�/@�/@��@�D@j@9X@�@1@1@1@ƨ@��@t�@S�@33@"�@
��@
�@	�@	�#@	�^@	��@	��@	��@	x�@	G�@	G�@	%@�`@�`@��@�9@r�@Q�@A�@1'@�@�@�P@l�@K�@K�@�@��@ȴ@ȴ@��@��@�+@v�@v�@v�@ff@ff@ff@ff@ff@ff@ff@V@@{@��@@��@@��@�@`B@`B@O�@?}@/@/@/@�@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��TA��HA��;A��yA��A��A��A��A��A���A���A��A���A��A���A�/APA�;dA�G�A�%A�(�A��/A�{A�O�A��hA�bA�~�A��wA���A��uA�ffA��9A��+A��/A�t�A���A�/A��A�%A��mA�;dA���A�A�A��wA�jA��A��TA�&�A��PA��PA��A�bA� �A���A��uA�K�A�-A�VA��A���A�~�A���A���A���A��
A�l�A�VA���A�~�A��A�33A�5?A���A���A��DA�ffA�A�p�A��A��A���A�"�A|��A{�^A{
=AzAx�Aw�Avr�At�As+AqƨAp�+AohsAn��Am��Al��Ak��Aj�9Ait�AhVAf�yAf1Ae�Ad��AcoAbI�Aa7LA^��A^E�A]�wA]��A]�A]A[XAY�hAXQ�AW�hAV��AU%ATbNASAQ��AQoAP��AP(�AOK�AN��AM�;AM�ALjAK�AJ�AI��AIVAH�AHJAG+AF��AEƨAEK�ADI�AC�-AC��ACC�AB��ABv�ABJA@5?A>��A=33A<�9A;�mA;XA:�yA:  A9l�A8�\A7�-A6��A6  A5��A5oA4�9A4��A3�TA2�A2z�A25?A1��A0ffA.�+A.(�A-�-A-7LA,�!A,1'A+O�A*$�A)C�A(ZA'��A&��A&$�A%�A$bA!VA bA�#A�A�A��An�A�#A?}AM�A�#A�Az�A9XA�
AXA�/AbNA��A"�AZAl�A��A��A �A�yA�;Az�A
r�A
5?A
$�A	�TA	��A	��A	7LAVA=qAbA��A33An�A��AK�AƨAS�A �+A -@��@�C�@��@��!@�5?@��7@�  @���@��@�bN@�t�@�ff@���@�;d@�@� �@@�X@��`@�1@�^5@�p�@蛦@��@�1@�\)@旍@�h@�r�@���@�x�@��@�K�@ް!@�@�/@���@�M�@ّh@�/@�I�@֧�@���@�I�@�=q@�hs@� �@�l�@��@θR@�M�@̓u@�l�@ʏ\@�@��@��;@�E�@���@�9X@�C�@¸R@�hs@���@�"�@�J@��-@��9@�\)@��R@�?}@�1@��+@��#@��h@�G�@��`@� �@�l�@�ȴ@�X@�/@��@��@�%@���@�dZ@�33@���@��/@�1'@��F@�t�@�\)@�"�@�
=@��y@��!@��\@�v�@�=q@��@�@���@�x�@�&�@���@��@���@���@�$�@��h@�7L@�&�@��@��j@��u@�Q�@��;@�t�@���@��-@�bN@��F@�@�
=@�n�@���@�hs@�Ĝ@�(�@�t�@�C�@�"�@��H@�E�@��T@��^@�`B@��@�K�@���@��@�v�@�J@���@��-@�x�@���@�bN@���@�C�@��!@���@��+@��@���@�Q�@��@�ƨ@���@�|�@��R@�^5@�$�@�$�@�{@�J@���@��@��/@���@� �@���@�ƨ@�\)@�33@�^5@��^@�%@�A�@��
@���@���@���@��@�l�@�K�@�@���@��@�Q�@��@��@��;@��
@��
@�dZ@��y@��@���@�=q@�n�@��@��7@�/@�/@�&�@���@��`@��@� �@��@�P@|�@l�@\)@
=@~v�@~ff@~{@}?}@|I�@{�
@{��@{dZ@{"�@zM�@y�^@y��@zJ@z�@|I�@|��@y�@x�`@xr�@x�u@w�;@v��@u�T@v$�@u��@up�@uO�@u�@u@v@u��@tz�@r��@rJ@q&�@q%@p�`@p��@pĜ@p��@p�@p1'@o�@ol�@oK�@o+@n�y@nff@m�-@m�@l��@lZ@l9X@k�F@kC�@k33@k@j�H@j��@jn�@j-@jJ@i�@iX@h��@hĜ@hĜ@h��@h�u@h�@hr�@h1'@gK�@f�+@fE�@e�@d��@d9X@d�@c��@c�F@c��@c33@b��@b�\@b^5@b�@a�@a�#@a��@a�^@a�7@aX@a%@`��@`��@`�`@a�@`��@`�9@`A�@`1'@_�;@_\)@_+@_
=@^ȴ@^��@^�+@^V@^$�@]�@]�@]O�@\��@\�/@\�@\j@[�m@[t�@["�@Z��@Z=q@Y�7@YX@X�9@Xb@W�@Xb@X1'@X  @W�;@W�w@W�@W�P@Wl�@W\)@Wl�@WK�@WK�@W�@V�@V��@V�+@VE�@V@U��@U�-@U�@T�@T�@Sƨ@S��@S��@S�F@S��@St�@St�@SC�@S@R�H@R�!@R=q@Q��@QG�@PĜ@PQ�@P  @O�;@O�;@O��@O��@O|�@O+@N��@N@Mp�@MO�@M/@L��@L��@L�@L��@LI�@L(�@L�@L�@K��@K33@K33@KC�@KC�@K@J��@J�!@J��@Jn�@I��@IG�@I%@HĜ@H�9@Hb@GK�@GK�@G;d@G;d@G;d@G+@G+@G�@F�y@F�+@FE�@F@EO�@EV@D�@D�@D1@C�@C"�@B��@B-@A��@A�@A�#@A�^@A��@Ax�@AG�@@�9@@ �@?�@?�w@?\)@>�@>�R@>�+@>$�@=�T@=�@<�/@;��@;t�@;C�@;"�@:�H@:�\@:=q@9X@9�@8�9@8�@8bN@8 �@7�@7�w@7�w@7�@7��@7|�@7K�@7K�@7;d@7
=@6�+@5@5�-@5�@4�@3�F@3S�@333@3o@2�@2��@2^5@1�@1�7@1X@0��@0Ĝ@0bN@/�@/\)@/K�@/�@.�y@.��@.5?@.@-�@-��@-�@-O�@,�@,��@,�@,��@,�D@,z�@,Z@+�
@+�F@+�@+33@+o@+@*�H@*�H@*^5@*J@)��@(�`@(Ĝ@(��@(�u@(r�@'�;@'�@'�P@'K�@';d@'+@&��@&��@&ff@&V@&E�@&5?@&$�@%�@%@%�h@%p�@%`B@%/@$�@$Z@$9X@$(�@$1@#�F@#��@#o@"-@!�#@!��@!X@!7L@!&�@!%@ ��@ �u@ A�@ b@��@+@�@�@�@�@
=@
=@��@�y@ff@E�@5?@$�@��@�-@��@�@?}@V@��@�D@1@�F@��@��@��@��@��@�@dZ@dZ@S�@S�@S�@C�@@��@��@��@�\@~�@n�@^5@J@�@��@�^@x�@�@��@�9@�u@�@r�@Q�@ �@b@�w@�P@\)@K�@
=@�y@�R@��@v�@v�@V@5?@{@�@��@�@?}@�@Z@(�@�@��@��@dZ@"�@�H@��@��@��@M�@J@�@��@X@�@�`@�u@�@�@r�@r�@r�@Q�@A�@1'@ �@��@
=@E�@��@@�-@�-@��@�h@p�@�@�/@�/@��@�D@j@9X@�@1@1@1@ƨ@��@t�@S�@33@"�@
��@
�@	�@	�#@	�^@	��@	��@	��@	x�@	G�@	G�@	%@�`@�`@��@�9@r�@Q�@A�@1'@�@�@�P@l�@K�@K�@�@��@ȴ@ȴ@��@��@�+@v�@v�@v�@ff@ff@ff@ff@ff@ff@ff@V@@{@��@@��@@��@�@`B@`B@O�@?}@/@/@/@�@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B1BB�sBǮB�dB�RB�9B�3B�B��B��B�3B�B��B�bB�\B�7B~�Bo�Be`B\)BN�BF�B;dB33B,B$�B�B�BuBPB  B��B�`B�
BĜB�3B�B�B�B�B��B��B��B�BgmBL�B;dB1'B-B&�B �B�BuB+B
��B
�B
�NB
�BB
�/B
��B
��B
ƨB
�XB
��B
�7B
o�B
e`B
_;B
W
B
O�B
F�B
<jB
49B
,B
#�B
�B
�B
hB
JB
%B	��B	��B	�B	�sB	�BB	�B	�B	��B	ƨB	��B	�^B	�B	��B	��B	��B	��B	��B	�VB	�%B	� B	y�B	r�B	k�B	hsB	dZB	]/B	W
B	S�B	P�B	L�B	J�B	F�B	@�B	;dB	6FB	0!B	+B	&�B	$�B	 �B	�B	�B	�B	{B	DB	+B	%B	B	B	B��B��B�B�yB�mB�ZB�NB�HB�;B�/B�B�B��B��BɺBǮBǮBɺBƨBÖB��B��B�wB�LB�!B�B�B�B��B��B��B��B��B��B�{B�oB�\B�JB�B~�B|�B{�B{�By�Bv�Bs�Bs�Bp�Bm�Bl�BiyBgmBe`BdZBbNB`BB^5B[#BYBYBW
BS�BQ�BM�BI�BE�BB�B@�B?}B?}B?}B>wB>wB<jB;dB;dB:^B9XB7LB6FB49B2-B0!B.B,B,B,B+B+B+B)�B(�B&�B%�B%�B%�B&�B&�B%�B%�B#�B �B �B!�B#�B"�B!�B�B �B �B�B!�B"�B#�B#�B#�B"�B#�B#�B"�B#�B$�B#�B!�B#�B"�B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B#�B$�B%�B&�B'�B'�B(�B+B-B.B/B/B1'B2-B49B6FB6FB8RB:^B;dB=qB>wBA�BB�BC�BC�BC�BE�BF�BH�BM�BM�BM�BM�BM�BO�BQ�BR�BR�BZB\)B]/B^5B^5B_;B_;B_;B_;B`BB`BB`BBaHBaHBbNBbNBcTBcTBcTBffBhsBjBl�Bm�Bn�Bm�Bo�Bo�Bp�Br�Bs�Bv�Bx�B{�B~�B�B�%B�1B�=B�DB�JB�VB�hB�oB�oB�uB��B��B��B��B��B��B�B�B�'B�FB�RB�XB�^B�wB��BĜBƨBǮBɺBɺB��B��B��B�B�B�B�B�5B�HB�TB�ZB�ZB�ZB�`B�yB�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	B	+B	1B	
=B	PB	\B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	#�B	#�B	'�B	)�B	,B	/B	/B	/B	0!B	1'B	2-B	5?B	7LB	8RB	8RB	9XB	:^B	;dB	>wB	>wB	?}B	D�B	H�B	I�B	J�B	K�B	K�B	Q�B	T�B	W
B	YB	^5B	dZB	hsB	dZB	bNB	bNB	dZB	jB	k�B	m�B	n�B	o�B	o�B	p�B	q�B	r�B	t�B	v�B	u�B	v�B	z�B	}�B	}�B	}�B	~�B	~�B	~�B	� B	� B	�B	�B	�B	�B	�B	�%B	�7B	�PB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�?B	�RB	�dB	�dB	�dB	�jB	�qB	�wB	�}B	��B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�;B	�;B	�BB	�HB	�TB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
	7B

=B
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
+B
+B
,B
,B
,B
-B
-B
.B
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
49B
49B
49B
5?B
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
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
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
G�B
H�B
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
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
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
S�B
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
VB
VB
VB
VB
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
XB
XB
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
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
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
_;B
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
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
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
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
u�B
u�B
u�B
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
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��BͼBͿB��B��B��B��B��B��B��B��B�B�1B� B�8B�B�B�BրB��B��B��B��B�UB�B��B��B��B�@B��B��B��B��Br�BhmBb+BSnBL2B>|B5�B-�B&�B �B�BAB�BB�(B��B�&B�B�nB��B�'B��B��B�sB�eB�B�MBp�BR�B?B38B.�B(�B!�B �B/B
B
��B
�kB
��B
�-B
�TB
֪B
��B
ʡB
�jB
�CB
�pB
rB
gB
a�B
Y�B
R�B
JoB
@B
8�B
/�B
''B
 �B
�B
�B
�B
DB
CB	��B	�B	��B	�kB	�zB	�KB	�rB	ȣB	�(B	�B	�}B	�SB	�>B	�1B	�NB	��B	��B	�VB	��B	|3B	v�B	m"B	jB	h�B	_AB	W�B	T�B	R�B	N#B	L:B	H6B	A�B	<�B	8�B	2B	,KB	'�B	&HB	"�B	 �B	 ,B	�B	�B	�B	`B		B	�B	wB	B	2B�bB��B�B�<B�B�RB�@B��B�'B�B�rB�[B˨B��BȈBǌB��B��BİB�2B�&B�B��B�	B�;B�9B�NB�0B�B��B��B��B�;B��B��B�B�8B�gB�eB}|B|�B}pB|�BzDBu=Bu[BsBn�Bn�BkBh,BfwBe�Bc�Ba�B_�B\�B[-B[�BY8BX�BS�BQBL�BI5BG�BA.B?�B@8B@B>�B?�B>�B;�B;�B;�B:�B9qB8B5�B6�B7	B01B,�B,�B,�B+bB+WB+�B*�B*�B)�B'5B&LB'B(AB) B'�B&�B&�B"�B"zB"mB%B$�B#B �B!{B �B �B"�B$;B%}B&B%�B$�B$�B$�B$B$�B&{B&B"�B$lB$B$�B#�B$�B%�B$B$}B$�B$NB$hB$zB&1B&gB'B(	B(�B)�B+,B,�B.B/]B/�B0�B2EB4 B5�B6�B7�B:B;<B=IB?B@uBBpBB�BDBD(BD�BF�BG�BJ�BNBM�BM�BM�BN�BP�BR)BSABUXBZ�B\�B]�B^^B^�B_eB_qB_�B_mB`mB`�B`�Ba�Ba|Bb�Bb�Bc�Bc�Bd�BgqBiUBkJBm	Bm�Bn�BnBo�Bo�Bq?BsEBt�Bw�BztB|�B�B�B��B��B�B�!B�B�BB��B��B��B�EB�B��B�$B��B�wB�bB�?B��B��B��B��B��B�B�BBB�B�jB��B��B�VB�<BѺBՈB�OB�GB�ZB�'B޶B�B�_B�zB�oB�B�YB��B��B�6B��B��B�LB�B�B��B��B�B	 �B	_B	#B	1B	=B	DB	[B	�B	
�B	�B	>B	B	�B	�B	�B	�B	;B	 YB	#�B	%0B	$ZB	#�B	(�B	*�B	,�B	/)B	/4B	/]B	0QB	1�B	2�B	5�B	7�B	8hB	8fB	9qB	:�B	;�B	>�B	>�B	@%B	EXB	IB	I�B	J�B	LB	LnB	RcB	UB	V�B	XsB	];B	dB	j�B	e%B	b�B	bHB	d�B	kzB	lB	mmB	n�B	o�B	o�B	p�B	q�B	r�B	uB	w�B	wB	wfB	{�B	~B	~B	~B	B	B	B	�IB	�kB	�HB	�7B	�5B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�(B	�B	��B	�B	�B	�B	�B	�:B	��B	��B	�wB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	·B	ìB	êB	ìB	��B	��B	��B	��B	��B	��B	ʰB	��B	�
B	�3B	�B	�-B	�OB	�/B	�)B	�DB	�2B	�8B	�HB	�FB	�PB	܈B	�[B	�nB	�aB	�iB	�yB	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�pB	�|B	�B	��B	�B	�B	�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	�KB	�>B	�ZB	�%B	�B	��B	��B	�B	�B	��B	�B	�2B	�!B
 7B
 hB
oB
�B
�B
�B
gB
HB
1B
EB
5B
{B
{B
�B
	�B

�B
kB
sB
zB
|B
vB
jB
�B
{B
mB
eB
�B
�B
gB
XB
gB
�B
�B
xB
zB
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
<B
�B
�B
�B
?B
/B
 B
 6B
!(B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"EB
#JB
$B
$	B
%5B
&TB
'B
'B
'?B
(/B
(GB
)�B
)�B
+mB
+2B
,/B
,GB
,NB
-XB
-�B
.VB
/oB
/JB
0HB
0aB
1VB
1RB
13B
1<B
2@B
2NB
2XB
29B
2DB
2XB
2�B
3�B
4OB
4fB
4�B
5�B
7�B
7mB
8sB
8tB
8�B
8�B
9�B
9�B
:�B
:�B
;�B
;�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
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
DB
D�B
D�B
EB
E�B
E�B
E�B
E�B
FB
G�B
G�B
G�B
H�B
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
J�B
J�B
J�B
J�B
J�B
L3B
L�B
L�B
L�B
MB
L�B
M8B
NxB
OB
PB
PB
QB
P�B
QB
QB
Q$B
Q,B
R!B
R,B
RvB
TB
TB
TB
TB
TB
TB
TB
TB
TiB
U"B
UB
UB
UJB
U$B
VB
V+B
V?B
V8B
VCB
WKB
WwB
XYB
X%B
X%B
XB
XB
X'B
X'B
X2B
XB
X'B
XB
XB
X*B
XMB
YJB
YHB
Y&B
Y.B
Y0B
Y0B
Y2B
YdB
ZCB
Z?B
Z5B
Z\B
[|B
[cB
\LB
\OB
\BB
\CB
\LB
\\B
]IB
]yB
]bB
]`B
^MB
^oB
^[B
^gB
_cB
__B
_IB
_]B
__B
__B
`gB
`jB
`�B
`�B
`�B
a�B
b�B
bhB
bxB
b�B
c�B
c�B
c�B
drB
dpB
d�B
d�B
e�B
e~B
e�B
e�B
f�B
f�B
f�B
g�B
gxB
g�B
gyB
gxB
g�B
g�B
g�B
g�B
g�B
h�B
i�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
n�B
p.B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v�B
wB
v�B
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
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<O�2<�o<z�t<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)(�<#�
<-T�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310930192018103109301920181031093019  AO  ARCAADJP                                                                    20180629170236    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180629170236  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180629170236  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031093019  QC  PRES            @&ffD�	�G�O�                PM  ARSQCTM V1.1                                                                20181031093019  QC  PSAL            @&ffD�	�G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230917  IP                  G�O�G�O�G�O�                