CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:41:23Z creation;2022-06-04T17:41:24Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174123  20220610131508  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               hA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @٩���=�1   @٩�/�c@/�|�hs�cQXbM�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B���B�  B���B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX33CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @.�R@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�\B��)B���B��)B���B��)B��)B�\B�\B�u�B�u�B��)B��)B��)B��)B��)B��)B��)B�\B��)BϨ�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C.�C/�zC1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CF�CG�CI�CK�CM�CO�CQ�CS�CU�CX!GCY�zC[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��=C��
C��=C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"uD"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�:�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�']111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aϟ�AϞ�Aϟ!AϢ4AϤ�Aϥ�AϦAϦAϦ�AϦ�Aϧ�Aϩ*AϪeAϪ0AϬ=AϭwAϯ�Aϯ�Aϰ�AϰUAϱ�Aϵ�AϵAϷ�A϶zA϶zAϽqA��'A��3A��3A���A�ȀA�ǮAϴA�8�A��A�;�A˄A��A�P�A��A��rAqA��A�&�A�33A��A�v�A��A�ZA�A��A���A��XA���A��A���A��BA�EmA���A�VA�<�A�w�A�qA���A�=A��PA�g�A��}A��%A�ffA��A�� A�}"A��@A��JA��A��A���A�e�A��A�M6A��_A��%A�e�A��EA�bA�
�A~_A{&�Ay�6Ax�FAu-�As/AqY�An&Adu�A`��A`#:A_�oA_�A_f�A^GEAWQ�AQJ#AM��AI�AES&AC��ABB[AA��AA_A@͟A?	�A:��A7�rA6�4A5 �A3��A2bA1�bA0g8A/X�A.�6A/[�A0;�A0�A/�QA/�YA.�2A.��A.g�A-��A,��A,��A+��A*��A)w2A(_�A%�DA#�UA"	�A�A �Av`A�A1�A��A��A��A��AA A�A�rA�A-�AB[A,=A�Aw2A�A�A�.AIRAںA�AtTA5?A��A9�A��A��A_AoA֡A�A!A�}Ac A�A@OAԕAP�A�A�$A��As�A�]A�MA��A{JAl�A?A��AzAiDAN�A�A
��A	�mA	GEAߤA�A��AA��A'�A�AA�A"�A�HA�hA�fA��A^�A�mA�A�$A=�A�.A˒A��A��Ay>A>BA��A�A8A ��A ��A f�@�:�@�E9@��*@�=@�IR@���@���@��[@���@�ƨ@��@�;d@��e@�-�@�@���@��u@��@��@�ϫ@�S�@�F�@�l�@�]d@�V@� i@�	@��@�c�@�q@��|@��@�^�@���@��o@�K�@��@��M@�e�@�b@�x�@��@�I�@���@�`B@��@�{�@�,=@��@�'@�7L@��@�C@��E@�^5@ߜ@��M@�{�@��z@���@�H�@�{@��r@��3@�:�@�2a@�"�@��@؝I@�O@׍P@�4@��@���@ӵt@��@�#:@Ѻ^@Ї�@��@��]@�Q�@ΐ.@��@ͷ�@�K�@��,@�\�@�4n@��@�J@��@ˬq@��@�B[@�=q@�M@��@�Q@ǎ"@ƾ@�@ōP@�L�@��M@�&�@��Q@æ�@�O@��m@x@�q@�R�@��@��m@�zx@��2@���@���@���@�-w@��v@���@���@�W�@�1'@�{@��@�Dg@���@�,=@��@�Z�@��4@�O@��[@���@�[W@���@��@�c�@���@�9�@��	@���@�2�@��@��f@��9@���@���@���@��@��@��	@��|@���@���@���@�m�@��-@��~@��@�u%@��@��M@��@��@���@�J@��-@�=�@��@�1'@�4@�{@��@���@��@�o�@�7L@�&@��f@��@�\�@��@��-@�%F@�͟@���@�h
@�0U@��M@��K@�ߤ@���@�xl@���@�Q�@���@�kQ@�
�@��#@��0@���@�j@�*�@��0@���@�c@�g�@�@�A�@��X@��@��P@��,@�s�@�@��@�;@�>B@��@@�/�@���@��y@��v@��]@�h
@�-�@�1@�X�@�;@���@��`@��R@�#:@���@�e�@�(�@��@��R@���@�\�@�0U@�1@���@���@���@�l�@�0�@��@�r�@�Ta@�e@��@���@�X@�:�@�8�@�@���@�B[@��r@��	@�>�@�/�@�Y@�ߤ@��'@��x@�z�@�bN@�6@��]@��@���@���@�n/@�ں@��}@���@��@�Q�@��@��@��@��@�O@��6@�B[@�~@�@�{J@�F@��@���@�)�@���@��a@��0@��C@�g�@�֡@��@���@�:�@��[@���@�zx@�j�@�J#@�(�@���@���@��@�I�@�O@��@��j@�U�@��@�ߤ@��m@��R@���@���@�6@��@��X@�,�@��@��$@���@�[�@�A�@�?�@�6@�O@�@��9@�e�@�@��@���@�6@�a@��@"�@~�m@~1�@}�-@}q@|4n@{��@{��@z�s@zQ@y�z@yzx@x�@xXy@w�Q@w_p@vu%@uB�@t�_@t!@s��@s��@sv`@r��@r!�@qf�@p��@pI�@o�;@o�k@o�@nں@n�F@nW�@n�@m��@m�n@mIR@l�f@l~(@l7@k��@kRT@j�@j�@jv�@j�@i�@i��@i`B@i*0@h�O@h,=@g��@giD@f��@fxl@eϫ@d�@d�@dm�@dl"@d[�@dx@ciD@c4�@c�@b�y@b�A@b!�@aO�@`�_@`]d@_�@_�V@_iD@_(@^�H@^�6@^B[@]@]�@]`B@]%@\�$@\?�@[�0@[=@Z�c@Z�@ZE�@Y��@Yw2@Y?}@XPH@W��@W�	@W�@Wx@W@O@V��@Vd�@U�@U|@UO�@U�@T��@T�_@T,=@T  @Sn/@S�@R��@R@�@Q��@Q�#@Q��@Qa�@P�P@P�$@PZ@PG@O�@O��@N��@M��@L֡@L��@Lc�@L$@K��@K��@J��@I�o@I�"@IA @H�@HK^@Gt�@GE9@GA�@G"�@G@F�@F��@E�@Ec@E%F@D��@C��@C�F@C��@C�@B��@B��@A��@A�@@�v@@�$@@tT@@"h@?خ@?~�@?�@>��@>�@=�@=@<��@<��@<_@<6@;�;@;�6@;��@;dZ@;6z@:��@:{�@:C�@:��@:W�@9��@9��@95�@8��@8�@8�.@8|�@8j@8D�@7�m@7�$@7S@6s�@6Ta@6J�@6E�@6@�@6;�@6&�@5�@5��@5rG@5:�@5V@4��@4��@4��@4z�@4_@4S�@4@3{J@3H�@3
=@2�}@2�F@2^5@2�@1�o@1��@1��@1|@1L�@1!�@0�|@0�@0m�@0*�@01@/��@/�@.��@.�6@..�@-��@-��@-��@-f�@-B�@,�j@,K^@,%�@+�+@+�@+��@+��@+qv@+&@*d�@*-@*&�@)��@)c@)^�@)+�@);@(�@(I�@(b@'�@'�0@'|�@'!-@&��@&� @&\�@&1�@&O@&�@%��@%��@%L�@$�|@$�[@$��@$D�@$@#� @#�@@#�k@#y�@#W?@#$t@"��@"\�@"@!��@!��@!!�@ ��@ m�@ 2�@ 	�@�w@iD@�@��@��@Z�@c @H�@�@��@�j@��@��@z�@ �@��@�&@�6@�@�@��@��@d�@W�@@�@@�9@��@�@��@�h@N<@ѷ@��@h�@˒@�@��@�@H�@!-@ i@�@z@Q@=q@:*@	@��@�"@`B@7L@*0@2a@2a@�K@�4@q@2�@�@��@�$@��@o�@_p@9�@C@�H@��@z@5?@�@ �@�>@�d@��@rG@k�@Vm@�@�)@�u@z�@e�@PH@K^@>B@C-@�+@�g@�g@��@�@�y@�,@�<@��@W�@E�@8�@-@($@4@�#@�~@f�@J�@ \@�@��@7�@�]@��@|�@l�@e�@]�@33@
=@ i@
��@
��@
��@
�r@
}V@
GE@
O@
_@	�@	��@	}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aϟ�AϞ�Aϟ!AϢ4AϤ�Aϥ�AϦAϦAϦ�AϦ�Aϧ�Aϩ*AϪeAϪ0AϬ=AϭwAϯ�Aϯ�Aϰ�AϰUAϱ�Aϵ�AϵAϷ�A϶zA϶zAϽqA��'A��3A��3A���A�ȀA�ǮAϴA�8�A��A�;�A˄A��A�P�A��A��rAqA��A�&�A�33A��A�v�A��A�ZA�A��A���A��XA���A��A���A��BA�EmA���A�VA�<�A�w�A�qA���A�=A��PA�g�A��}A��%A�ffA��A�� A�}"A��@A��JA��A��A���A�e�A��A�M6A��_A��%A�e�A��EA�bA�
�A~_A{&�Ay�6Ax�FAu-�As/AqY�An&Adu�A`��A`#:A_�oA_�A_f�A^GEAWQ�AQJ#AM��AI�AES&AC��ABB[AA��AA_A@͟A?	�A:��A7�rA6�4A5 �A3��A2bA1�bA0g8A/X�A.�6A/[�A0;�A0�A/�QA/�YA.�2A.��A.g�A-��A,��A,��A+��A*��A)w2A(_�A%�DA#�UA"	�A�A �Av`A�A1�A��A��A��A��AA A�A�rA�A-�AB[A,=A�Aw2A�A�A�.AIRAںA�AtTA5?A��A9�A��A��A_AoA֡A�A!A�}Ac A�A@OAԕAP�A�A�$A��As�A�]A�MA��A{JAl�A?A��AzAiDAN�A�A
��A	�mA	GEAߤA�A��AA��A'�A�AA�A"�A�HA�hA�fA��A^�A�mA�A�$A=�A�.A˒A��A��Ay>A>BA��A�A8A ��A ��A f�@�:�@�E9@��*@�=@�IR@���@���@��[@���@�ƨ@��@�;d@��e@�-�@�@���@��u@��@��@�ϫ@�S�@�F�@�l�@�]d@�V@� i@�	@��@�c�@�q@��|@��@�^�@���@��o@�K�@��@��M@�e�@�b@�x�@��@�I�@���@�`B@��@�{�@�,=@��@�'@�7L@��@�C@��E@�^5@ߜ@��M@�{�@��z@���@�H�@�{@��r@��3@�:�@�2a@�"�@��@؝I@�O@׍P@�4@��@���@ӵt@��@�#:@Ѻ^@Ї�@��@��]@�Q�@ΐ.@��@ͷ�@�K�@��,@�\�@�4n@��@�J@��@ˬq@��@�B[@�=q@�M@��@�Q@ǎ"@ƾ@�@ōP@�L�@��M@�&�@��Q@æ�@�O@��m@x@�q@�R�@��@��m@�zx@��2@���@���@���@�-w@��v@���@���@�W�@�1'@�{@��@�Dg@���@�,=@��@�Z�@��4@�O@��[@���@�[W@���@��@�c�@���@�9�@��	@���@�2�@��@��f@��9@���@���@���@��@��@��	@��|@���@���@���@�m�@��-@��~@��@�u%@��@��M@��@��@���@�J@��-@�=�@��@�1'@�4@�{@��@���@��@�o�@�7L@�&@��f@��@�\�@��@��-@�%F@�͟@���@�h
@�0U@��M@��K@�ߤ@���@�xl@���@�Q�@���@�kQ@�
�@��#@��0@���@�j@�*�@��0@���@�c@�g�@�@�A�@��X@��@��P@��,@�s�@�@��@�;@�>B@��@@�/�@���@��y@��v@��]@�h
@�-�@�1@�X�@�;@���@��`@��R@�#:@���@�e�@�(�@��@��R@���@�\�@�0U@�1@���@���@���@�l�@�0�@��@�r�@�Ta@�e@��@���@�X@�:�@�8�@�@���@�B[@��r@��	@�>�@�/�@�Y@�ߤ@��'@��x@�z�@�bN@�6@��]@��@���@���@�n/@�ں@��}@���@��@�Q�@��@��@��@��@�O@��6@�B[@�~@�@�{J@�F@��@���@�)�@���@��a@��0@��C@�g�@�֡@��@���@�:�@��[@���@�zx@�j�@�J#@�(�@���@���@��@�I�@�O@��@��j@�U�@��@�ߤ@��m@��R@���@���@�6@��@��X@�,�@��@��$@���@�[�@�A�@�?�@�6@�O@�@��9@�e�@�@��@���@�6@�a@��@"�@~�m@~1�@}�-@}q@|4n@{��@{��@z�s@zQ@y�z@yzx@x�@xXy@w�Q@w_p@vu%@uB�@t�_@t!@s��@s��@sv`@r��@r!�@qf�@p��@pI�@o�;@o�k@o�@nں@n�F@nW�@n�@m��@m�n@mIR@l�f@l~(@l7@k��@kRT@j�@j�@jv�@j�@i�@i��@i`B@i*0@h�O@h,=@g��@giD@f��@fxl@eϫ@d�@d�@dm�@dl"@d[�@dx@ciD@c4�@c�@b�y@b�A@b!�@aO�@`�_@`]d@_�@_�V@_iD@_(@^�H@^�6@^B[@]@]�@]`B@]%@\�$@\?�@[�0@[=@Z�c@Z�@ZE�@Y��@Yw2@Y?}@XPH@W��@W�	@W�@Wx@W@O@V��@Vd�@U�@U|@UO�@U�@T��@T�_@T,=@T  @Sn/@S�@R��@R@�@Q��@Q�#@Q��@Qa�@P�P@P�$@PZ@PG@O�@O��@N��@M��@L֡@L��@Lc�@L$@K��@K��@J��@I�o@I�"@IA @H�@HK^@Gt�@GE9@GA�@G"�@G@F�@F��@E�@Ec@E%F@D��@C��@C�F@C��@C�@B��@B��@A��@A�@@�v@@�$@@tT@@"h@?خ@?~�@?�@>��@>�@=�@=@<��@<��@<_@<6@;�;@;�6@;��@;dZ@;6z@:��@:{�@:C�@:��@:W�@9��@9��@95�@8��@8�@8�.@8|�@8j@8D�@7�m@7�$@7S@6s�@6Ta@6J�@6E�@6@�@6;�@6&�@5�@5��@5rG@5:�@5V@4��@4��@4��@4z�@4_@4S�@4@3{J@3H�@3
=@2�}@2�F@2^5@2�@1�o@1��@1��@1|@1L�@1!�@0�|@0�@0m�@0*�@01@/��@/�@.��@.�6@..�@-��@-��@-��@-f�@-B�@,�j@,K^@,%�@+�+@+�@+��@+��@+qv@+&@*d�@*-@*&�@)��@)c@)^�@)+�@);@(�@(I�@(b@'�@'�0@'|�@'!-@&��@&� @&\�@&1�@&O@&�@%��@%��@%L�@$�|@$�[@$��@$D�@$@#� @#�@@#�k@#y�@#W?@#$t@"��@"\�@"@!��@!��@!!�@ ��@ m�@ 2�@ 	�@�w@iD@�@��@��@Z�@c @H�@�@��@�j@��@��@z�@ �@��@�&@�6@�@�@��@��@d�@W�@@�@@�9@��@�@��@�h@N<@ѷ@��@h�@˒@�@��@�@H�@!-@ i@�@z@Q@=q@:*@	@��@�"@`B@7L@*0@2a@2a@�K@�4@q@2�@�@��@�$@��@o�@_p@9�@C@�H@��@z@5?@�@ �@�>@�d@��@rG@k�@Vm@�@�)@�u@z�@e�@PH@K^@>B@C-@�+@�g@�g@��@�@�y@�,@�<@��@W�@E�@8�@-@($@4@�#@�~@f�@J�@ \@�@��@7�@�]@��@|�@l�@e�@]�@33@
=@ i@
��@
��@
��@
�r@
}V@
GE@
O@
_@	�@	��@	}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�jB
�jB
�jB
�PB
�jB
�B
�6B
�6B
�PB
̈́B
̈́B
͟B
͹B
͟B
͟B
͹B
�6B
͹B
͟B
�PB
�jB
�B
�PB
�6B
�jB
̈́B
�B
�B
��B
�B
͹B
��B
�vB
�GBY�By	B�=B��B�qB��B��B��B�%B�xB��B��B�]B�wB��B�TB��B�bB�B��B�JB$B%`B1'B�B��B�`B�OB��B�OB��B��B��B��Bg�BD�BBB#B#�B-�B,B&B�B
�TB
�B
�XB
ߊB
��B
�bB
�4B
t�B
d�B
.B	�wB	�B	�&B	ݲB	��B	�wB	��B	��B	q�B	Z�B	V�B	U�B	UB	TFB	MjB	49B	eB	BB	
�B	�B	�B	�B	�B	yB		B	&�B	J�B	Z�B	^jB	b�B	b�B	g�B	j�B	wB	�RB	�MB	��B	��B	��B	�B	�B	چB	��B	��B	�B	�1B	�B	�B	�B	�-B	ߤB	̳B	��B	�	B	��B	�-B	��B	��B	��B	�!B	�/B	��B	�IB	��B	�B	��B	�<B	��B	�CB	��B	�,B	�B	�B	�mB	�B	�fB	�B	�>B	�tB	��B	��B	�B	��B	�qB	��B	��B	��B	�HB	��B	��B	�B	��B	��B	�B	��B	��B	�]B
 4B
�B
 4B
  B
 OB	��B	��B
UB
 �B	��B
 B
�B
;B
AB
+B

�B
B
0B

�B

�B
�B
�B
PB
�B

�B
�B
_B
YB
�B
�B
B
oB
UB
 iB	��B	��B	��B	�]B	��B	�VB	�B	��B
 �B
KB
�B
VB
B
;B	��B	�0B	��B
 �B
�B
�B
�B
BB
HB
�B
bB
vB
B
~B

XB
�B
�B
gB
�B
�B
�B
	�B
B
�B
 �B	�jB	�RB	�B	�B	��B	�5B	�iB	��B	�B	�CB	�/B	�'B	��B	��B	�B	��B	��B	�OB	��B	�CB	��B	�B	�B	�B	�QB	�CB	�wB	�B	�B	�6B	�"B	��B	�*B	��B	��B	�\B	ߊB	��B	ߤB	�pB	ߊB	߾B	�HB	�B	�&B	�B	�&B	�B	��B	��B	�|B	�B	�;B	�;B	��B	��B	�bB	�B	�NB	�:B	�ZB	��B	�`B	�zB	�`B	�B	�8B	�B	�B	�B	�B	�
B	�B	�B	�>B	�mB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�eB	�B	�B	�CB	�}B	�IB	�oB	�B	�-B	�B	�TB	�tB	�B	�B	��B	�?B	��B	�FB	��B	��B	�>B	��B	��B	��B	�PB	�jB	�VB	��B	�JB	�*B	��B	�rB	�>B	�$B	��B	�(B	�HB	��B
  B
 �B
 �B
 �B
 �B
 �B
 �B	�B	��B	�^B	��B	��B	��B	��B	��B	�B	�qB
aB
oB	�}B
  B	�cB	�(B	�B	��B	��B	��B	��B
B
AB
B
�B
�B
[B
�B
�B
;B
 4B
 �B
 �B
;B
�B
 �B
B
B
oB
oB
 �B
 �B
�B
�B
oB
 �B
oB
 �B
;B
 B
�B
-B
�B
B
gB
�B
9B
B
B
�B
�B
%B
�B
�B
�B
�B
	7B
	lB
	�B

=B
�B
JB
0B
�B
dB
�B
�B
�B
�B
jB
B
�B
BB
bB
B
[B
@B
B
�B
�B
[B
�B
FB
B
sB
�B
B
�B
QB
�B
�B
�B
CB
xB
B
/B
/B
IB
IB
dB
dB
~B
�B
~B
~B
�B
B
�B
VB
pB
�B
!HB
!bB
!bB
!�B
!�B
"NB
"4B
"NB
"�B
"�B
"�B
"hB
"�B
#B
# B
#nB
#�B
#�B
%,B
%`B
%zB
%zB
%`B
%�B
'B
'B
'B
'�B
(�B
(�B
(�B
(�B
)yB
)�B
)�B
)�B
+B
+QB
+�B
+�B
+�B
-)B
-]B
-�B
-�B
-�B
-�B
-�B
.�B
/ B
/OB
0!B
0UB
0;B
0!B
0�B
0�B
0�B
0�B
0�B
1B
1[B
1�B
2B
2-B
2GB
2|B
2�B
2�B
3MB
3�B
4�B
4�B
5%B
6zB
6FB
6�B
6zB
6`B
6FB
6�B
72B
7�B
7fB
7�B
7fB
7LB
7�B
7�B
7�B
7�B
7�B
88B
8�B
8�B
9rB
9�B
:DB
:*B
:^B
:^B
:�B
:�B
;dB
;dB
;�B
;�B
;�B
<6B
<�B
=B
=<B
=�B
=�B
=�B
>BB
>BB
>wB
>]B
>�B
>�B
?cB
?�B
?�B
@OB
@�B
A;B
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
C-B
CB
CGB
C{B
C�B
C�B
D3B
DgB
D�B
D�B
EB
EmB
E�B
E�B
E�B
F?B
F?B
F�B
G+B
GzB
GzB
G�B
G�B
HKB
HfB
H�B
IlB
I�B
I�B
I�B
I�B
I�B
J	B
JrB
J�B
K)B
KDB
KDB
K^B
K�B
K�B
LB
L�B
L�B
MPB
MPB
M�B
M�B
M�B
M�B
N"B
N<B
N�B
N�B
N�B
N�B
OvB
PB
P�B
P�B
Q B
Q4B
QhB
QhB
RB
RoB
R�B
R�B
R�B
SB
S[B
S[B
S[B
SuB
SuB
S�B
S@B
T�B
T�B
T�B
UMB
U�B
VB
U�B
V9B
VSB
VmB
WsB
W�B
W�B
W�B
XB
XEB
X+B
XyB
X�B
X�B
XEB
Y1B
YB
Y�B
Y�B
ZB
Y�B
ZB
Y�B
ZB
Y�B
Y�B
Y1B
YKB
Y�B
Z�B
Z�B
[�B
[�B
\B
\B
\]B
\CB
\CB
\)B
\CB
\)B
\)B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]IB
]IB
]/B
]dB
]�B
]�B
]�B
^B
]�B
^5B
^�B
^�B
^�B
_VB
_;B
_�B
_�B
`B
_�B
`\B
`vB
`�B
`�B
`�B
`�B
aHB
a�B
a�B
b4B
b�B
b�B
b�B
cnB
cnB
c�B
c�B
c�B
c�B
d�B
eB
eB
e,B
e`B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
gRB
g8B
gmB
g�B
h$B
h>B
hsB
h�B
h�B
iB
iyB
i�B
i�B
i�B
j0B
jB
jKB
jeB
jKB
j�B
kQB
kB
kB
kB
kB
kB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
lB
lqB
l�B
l�B
mB
m]B
mwB
m�B
nB
ncB
oB
oOB
oiB
o�B
o�B
o�B
oOB
pB
p�B
p�B
qB
qvB
q�B
qvB
q�B
q�B
rGB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
t9B
tTB
t�B
u%B
u%B
uZB
u�B
vFB
vB
vB
vB
w2B
wfB
w�B
w�B
wfB
wfB
w2B
w�B
w�B
xB
xB
xB
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y$B
y>B
yrB
yrB
y�B
z^B
zDB
z�B
z�B
z�B
{B
{JB
|6B
|jB
|jB
|�B
}B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
}qB
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�B
�B
�B
�B
�OB
�OB
��B
��B
��B
�B
�UB
��B
�B
�'B
�AB
�'B
�'B
�uB
��B
��B
��B
��B
�B
�B
�B
�-B
�GB
�{B
�{B
�aB
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�jB
�jB
�jB
�PB
�jB
�B
�6B
�6B
�PB
̈́B
̈́B
͟B
͹B
͟B
͟B
͹B
�6B
͹B
͟B
�PB
�jB
�B
�PB
�6B
�jB
̈́B
�B
�B
��B
�B
͹B
��B
�vB
�GBY�By	B�=B��B�qB��B��B��B�%B�xB��B��B�]B�wB��B�TB��B�bB�B��B�JB$B%`B1'B�B��B�`B�OB��B�OB��B��B��B��Bg�BD�BBB#B#�B-�B,B&B�B
�TB
�B
�XB
ߊB
��B
�bB
�4B
t�B
d�B
.B	�wB	�B	�&B	ݲB	��B	�wB	��B	��B	q�B	Z�B	V�B	U�B	UB	TFB	MjB	49B	eB	BB	
�B	�B	�B	�B	�B	yB		B	&�B	J�B	Z�B	^jB	b�B	b�B	g�B	j�B	wB	�RB	�MB	��B	��B	��B	�B	�B	چB	��B	��B	�B	�1B	�B	�B	�B	�-B	ߤB	̳B	��B	�	B	��B	�-B	��B	��B	��B	�!B	�/B	��B	�IB	��B	�B	��B	�<B	��B	�CB	��B	�,B	�B	�B	�mB	�B	�fB	�B	�>B	�tB	��B	��B	�B	��B	�qB	��B	��B	��B	�HB	��B	��B	�B	��B	��B	�B	��B	��B	�]B
 4B
�B
 4B
  B
 OB	��B	��B
UB
 �B	��B
 B
�B
;B
AB
+B

�B
B
0B

�B

�B
�B
�B
PB
�B

�B
�B
_B
YB
�B
�B
B
oB
UB
 iB	��B	��B	��B	�]B	��B	�VB	�B	��B
 �B
KB
�B
VB
B
;B	��B	�0B	��B
 �B
�B
�B
�B
BB
HB
�B
bB
vB
B
~B

XB
�B
�B
gB
�B
�B
�B
	�B
B
�B
 �B	�jB	�RB	�B	�B	��B	�5B	�iB	��B	�B	�CB	�/B	�'B	��B	��B	�B	��B	��B	�OB	��B	�CB	��B	�B	�B	�B	�QB	�CB	�wB	�B	�B	�6B	�"B	��B	�*B	��B	��B	�\B	ߊB	��B	ߤB	�pB	ߊB	߾B	�HB	�B	�&B	�B	�&B	�B	��B	��B	�|B	�B	�;B	�;B	��B	��B	�bB	�B	�NB	�:B	�ZB	��B	�`B	�zB	�`B	�B	�8B	�B	�B	�B	�B	�
B	�B	�B	�>B	�mB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�eB	�B	�B	�CB	�}B	�IB	�oB	�B	�-B	�B	�TB	�tB	�B	�B	��B	�?B	��B	�FB	��B	��B	�>B	��B	��B	��B	�PB	�jB	�VB	��B	�JB	�*B	��B	�rB	�>B	�$B	��B	�(B	�HB	��B
  B
 �B
 �B
 �B
 �B
 �B
 �B	�B	��B	�^B	��B	��B	��B	��B	��B	�B	�qB
aB
oB	�}B
  B	�cB	�(B	�B	��B	��B	��B	��B
B
AB
B
�B
�B
[B
�B
�B
;B
 4B
 �B
 �B
;B
�B
 �B
B
B
oB
oB
 �B
 �B
�B
�B
oB
 �B
oB
 �B
;B
 B
�B
-B
�B
B
gB
�B
9B
B
B
�B
�B
%B
�B
�B
�B
�B
	7B
	lB
	�B

=B
�B
JB
0B
�B
dB
�B
�B
�B
�B
jB
B
�B
BB
bB
B
[B
@B
B
�B
�B
[B
�B
FB
B
sB
�B
B
�B
QB
�B
�B
�B
CB
xB
B
/B
/B
IB
IB
dB
dB
~B
�B
~B
~B
�B
B
�B
VB
pB
�B
!HB
!bB
!bB
!�B
!�B
"NB
"4B
"NB
"�B
"�B
"�B
"hB
"�B
#B
# B
#nB
#�B
#�B
%,B
%`B
%zB
%zB
%`B
%�B
'B
'B
'B
'�B
(�B
(�B
(�B
(�B
)yB
)�B
)�B
)�B
+B
+QB
+�B
+�B
+�B
-)B
-]B
-�B
-�B
-�B
-�B
-�B
.�B
/ B
/OB
0!B
0UB
0;B
0!B
0�B
0�B
0�B
0�B
0�B
1B
1[B
1�B
2B
2-B
2GB
2|B
2�B
2�B
3MB
3�B
4�B
4�B
5%B
6zB
6FB
6�B
6zB
6`B
6FB
6�B
72B
7�B
7fB
7�B
7fB
7LB
7�B
7�B
7�B
7�B
7�B
88B
8�B
8�B
9rB
9�B
:DB
:*B
:^B
:^B
:�B
:�B
;dB
;dB
;�B
;�B
;�B
<6B
<�B
=B
=<B
=�B
=�B
=�B
>BB
>BB
>wB
>]B
>�B
>�B
?cB
?�B
?�B
@OB
@�B
A;B
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
C-B
CB
CGB
C{B
C�B
C�B
D3B
DgB
D�B
D�B
EB
EmB
E�B
E�B
E�B
F?B
F?B
F�B
G+B
GzB
GzB
G�B
G�B
HKB
HfB
H�B
IlB
I�B
I�B
I�B
I�B
I�B
J	B
JrB
J�B
K)B
KDB
KDB
K^B
K�B
K�B
LB
L�B
L�B
MPB
MPB
M�B
M�B
M�B
M�B
N"B
N<B
N�B
N�B
N�B
N�B
OvB
PB
P�B
P�B
Q B
Q4B
QhB
QhB
RB
RoB
R�B
R�B
R�B
SB
S[B
S[B
S[B
SuB
SuB
S�B
S@B
T�B
T�B
T�B
UMB
U�B
VB
U�B
V9B
VSB
VmB
WsB
W�B
W�B
W�B
XB
XEB
X+B
XyB
X�B
X�B
XEB
Y1B
YB
Y�B
Y�B
ZB
Y�B
ZB
Y�B
ZB
Y�B
Y�B
Y1B
YKB
Y�B
Z�B
Z�B
[�B
[�B
\B
\B
\]B
\CB
\CB
\)B
\CB
\)B
\)B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]IB
]IB
]/B
]dB
]�B
]�B
]�B
^B
]�B
^5B
^�B
^�B
^�B
_VB
_;B
_�B
_�B
`B
_�B
`\B
`vB
`�B
`�B
`�B
`�B
aHB
a�B
a�B
b4B
b�B
b�B
b�B
cnB
cnB
c�B
c�B
c�B
c�B
d�B
eB
eB
e,B
e`B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
gRB
g8B
gmB
g�B
h$B
h>B
hsB
h�B
h�B
iB
iyB
i�B
i�B
i�B
j0B
jB
jKB
jeB
jKB
j�B
kQB
kB
kB
kB
kB
kB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
lB
lqB
l�B
l�B
mB
m]B
mwB
m�B
nB
ncB
oB
oOB
oiB
o�B
o�B
o�B
oOB
pB
p�B
p�B
qB
qvB
q�B
qvB
q�B
q�B
rGB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
t9B
tTB
t�B
u%B
u%B
uZB
u�B
vFB
vB
vB
vB
w2B
wfB
w�B
w�B
wfB
wfB
w2B
w�B
w�B
xB
xB
xB
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y$B
y>B
yrB
yrB
y�B
z^B
zDB
z�B
z�B
z�B
{B
{JB
|6B
|jB
|jB
|�B
}B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
}qB
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�B
�B
�B
�B
�OB
�OB
��B
��B
��B
�B
�UB
��B
�B
�'B
�AB
�'B
�'B
�uB
��B
��B
��B
��B
�B
�B
�B
�-B
�GB
�{B
�{B
�aB
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104927  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174123  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174124  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174124                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024131  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024131  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131508                      G�O�G�O�G�O�                