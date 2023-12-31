CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-28T00:35:35Z creation;2018-06-28T00:35:39Z conversion to V3.1;2019-12-19T07:38:49Z update;     
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
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20180628003535  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_254                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�m�L
��1   @�m��l @9�u%F
��dD�Ϫ͟1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A���A�  A�33A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@�@�A�HA>�HA^�HA~�HA�p�A�=qA�p�A���A�p�A�p�A��A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��=C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�O�A�;dA�;dA�7LA�5?A�7LA�&�A�
=A�ƨA�|�A��A���A�bNA�9XA�/A��A��A�E�A�A��7A�ZA�ȴA�A�G�A���A�{A���A�{A�l�A���A���A�VA���A��/A�M�A���A�S�A�Q�A�
=A�{A�A�A�
=A���A�K�A�p�A� �A�G�A�`BA�=qA�;dA�JA�(�A��A�/A�(�A���A�jA���A��A�?}A��A��!A���A�z�A���A���A�=qA���A�ĜA�\)A��uA�=qA���A�~�A�7LA��A�ĜA��HA�t�A~�uA}�A{%Aw�^Av=qAuoAs��As+Ar�uAq��Ao��An��Am�hAmC�AmVAk��Aj��Ai�TAh�yAg��Ag7LAf��Af �Ae�7Ad��AcAct�Ab�Aa�#Aax�A`�A_K�A]��A]�A\ffA[�7AZ��AY|�AW�hAU�^AT �AS�#AR9XAPM�AN�AN�ANn�AM��AL�uAK�7AKS�AK&�AJVAJAI�-AH��AF��AD�ACx�AA�AA��AAx�AAO�A@�`A@VA?33A<$�A9��A8�yA8�A8�\A8�A8~�A8Q�A8-A81A7��A7��A7�A4�+A1�;A1O�A0��A0�uA09XA/��A/oA-��A-+A,�9A,(�A+�A+33A* �A)�
A)hsA)%A(�yA(ȴA(�uA(^5A($�A'�A'K�A& �A%&�A$��A$M�A$$�A#�wA#dZA#33A"��A!�mA!��A!�A ��A �+A �A
=A�wA�DAt�A��A9XA�AVAz�A=qA1A|�AA�A~�A��A�HA^5AVA�A��A-A��AA{AA��A`BA�A
��A
ȴA
��A
�DA
A�A	�#A5?A�HAx�A �A33AI�A�A��At�AK�A A�@���@��@�V@�z�@�dZ@��T@�n�@�bN@�33@��@��/@�j@�  @�S�@�7L@��@�l�@���@�E�@��@�9X@�@��/@�|�@�"�@ް!@��@� �@��@�\)@�^5@թ�@�&�@Ԭ@�b@��#@�Q�@��@�{@͉7@̴9@�b@�;d@�|�@�&�@��@�Ĝ@�1@¸R@��#@�(�@��\@�hs@���@�Z@���@��\@���@���@���@�hs@�hs@�O�@���@��D@�A�@��;@��y@��+@�E�@�J@�V@�1'@��
@��y@�J@���@�A�@���@�|�@�"�@�
=@��@�@��@��w@�E�@���@���@��`@�ƨ@���@���@��@�O�@��9@�A�@��@�\)@��@��+@�^5@�$�@��T@���@�`B@�/@�z�@��@�-@�=q@�x�@��u@�K�@��@�~�@�J@���@��h@�O�@��D@�  @��@���@�"�@�v�@��^@��@�?}@���@���@��@�l�@�S�@���@�ff@���@�p�@�V@�bN@� �@��F@���@�l�@�K�@�;d@��@��R@�M�@���@���@���@�x�@�`B@��@��@��;@���@��7@�`B@��@��9@���@��u@�r�@�I�@� �@�  @��
@��@���@��@�t�@�dZ@�S�@�;d@�o@�
=@�@��@��H@��R@���@�~�@�^5@�J@���@�G�@���@���@��`@���@��/@��/@��/@���@���@��@�A�@+@}O�@|��@|(�@|(�@|9X@{��@{��@{S�@{o@{@z�@z�H@z��@z��@z��@z�!@z��@z�\@z^5@y�#@y�^@y��@x�`@xb@w+@vE�@u�-@u`B@u?}@u�@uV@t��@s@r�\@r^5@rJ@q�#@q�7@q7L@p��@pr�@pA�@p1'@ol�@n�@n�R@n�R@n�+@m�@l�@l�@k�m@k�
@kƨ@k��@kt�@kC�@ko@j��@j~�@j=q@i�^@i�@h�9@h�u@hbN@hA�@hb@g�@g�;@g\)@fv�@f@e@e�-@e�h@e�h@e�h@e�@e`B@d��@dj@dI�@d�@c�@bn�@b�@a�#@a�#@a��@a��@a��@ax�@a%@`��@`�@`1'@_��@_��@_l�@_l�@_\)@_K�@_�@_
=@^��@^�@^��@^V@]�T@]`B@]?}@]�@\�@\�/@\�/@\�j@\�@\z�@\Z@\I�@\�@[�m@[�@[C�@[o@Z~�@Y��@Y��@Y�7@Yx�@YX@YG�@Y�@X�`@XĜ@XbN@W\)@V�R@Vff@Up�@T�/@T�j@T9X@S�@R�@R��@R��@R�!@R��@R^5@RM�@R=q@R�@RJ@Q�@Q��@Q��@QX@QG�@Q�@P�u@PbN@PQ�@Pb@O\)@N�+@Nff@N5?@N{@M��@MO�@L�@L�j@Lj@L�@K�F@K��@K�@KdZ@K33@J�\@J=q@I�#@Ihs@H�`@H  @G��@G�@Gl�@G
=@F�+@F{@E@E�h@EO�@E?}@EV@D��@D�@D�@D�/@D�D@D9X@C�@C"�@C@B�@B�@B�@B��@B��@B�\@BM�@A�^@A&�@@�`@@A�@?��@?K�@?�@?
=@>��@=�@=�@=/@<�@;�m@;S�@;C�@;C�@;@:^5@:-@:J@9�^@9x�@97L@9�@8��@8��@8�9@7�@7l�@7K�@7;d@7
=@6�@6�+@6v�@6ff@6E�@5�@5`B@5V@4��@4�@4�@4��@4Z@3��@3�@3�@3S�@3C�@3o@2�@2��@2�!@2-@1�^@1x�@1X@1G�@17L@0��@0�9@0bN@0Q�@01'@/��@/\)@.��@.$�@-�T@-O�@-V@,��@,�@,�@,��@,��@,��@,�D@,�D@,z�@,z�@,Z@,I�@,I�@,�@+�
@+��@+�@+33@*��@*��@)�#@)hs@)%@(�`@(�9@(��@(�u@(r�@'�@'�@'l�@'�@&�y@&ȴ@&ȴ@&�R@&��@%�@%��@%�@$�D@$9X@$9X@$9X@$(�@$1@#��@#S�@#33@#33@#@"�!@"n�@"J@!�#@!��@!�7@!x�@!hs@!&�@!%@ �`@ �@ A�@�P@\)@K�@+@�y@�R@�+@E�@�@�@�-@�h@�@`B@�@�j@�D@Z@I�@1@ƨ@�@33@o@�H@��@��@�\@n�@-@��@��@�@�`@�`@��@�@�;@��@|�@\)@;d@�@�y@��@�+@ff@5?@�@@��@�@�@`B@�@�@z�@(�@��@�m@�m@ƨ@��@S�@o@��@�\@^5@�@J@J@�@�#@�7@7L@�@%@��@%@%@��@��@r�@�@�P@K�@
=@�@E�@�-@/@��@�/@��@�j@��@j@Z@9X@9X@(�@�@1@��@ƨ@��@dZ@
�@
�!@
��@
��@
�\@
n�@
n�@
n�@
M�@
-@	�@	�#@	��@	�^@	��@	�7@	X@	X@	G�@	7L@	%@��@bN@Q�@Q�@A�@A�@A�@A�@A�@1'@ �@�@K�@�@�R@�R@��@v�@v�@ff@5?@�@@��@p�@`B@O�@/@/@�@/@�@�@/@�@��@��@�D@j@Z@I�@I�@9X@(�@�@��@�@dZ@S�@33@o@�!@�\@n�@~�@n�@^5@M�@=q@=q@-@-@-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�O�A�;dA�;dA�7LA�5?A�7LA�&�A�
=A�ƨA�|�A��A���A�bNA�9XA�/A��A��A�E�A�A��7A�ZA�ȴA�A�G�A���A�{A���A�{A�l�A���A���A�VA���A��/A�M�A���A�S�A�Q�A�
=A�{A�A�A�
=A���A�K�A�p�A� �A�G�A�`BA�=qA�;dA�JA�(�A��A�/A�(�A���A�jA���A��A�?}A��A��!A���A�z�A���A���A�=qA���A�ĜA�\)A��uA�=qA���A�~�A�7LA��A�ĜA��HA�t�A~�uA}�A{%Aw�^Av=qAuoAs��As+Ar�uAq��Ao��An��Am�hAmC�AmVAk��Aj��Ai�TAh�yAg��Ag7LAf��Af �Ae�7Ad��AcAct�Ab�Aa�#Aax�A`�A_K�A]��A]�A\ffA[�7AZ��AY|�AW�hAU�^AT �AS�#AR9XAPM�AN�AN�ANn�AM��AL�uAK�7AKS�AK&�AJVAJAI�-AH��AF��AD�ACx�AA�AA��AAx�AAO�A@�`A@VA?33A<$�A9��A8�yA8�A8�\A8�A8~�A8Q�A8-A81A7��A7��A7�A4�+A1�;A1O�A0��A0�uA09XA/��A/oA-��A-+A,�9A,(�A+�A+33A* �A)�
A)hsA)%A(�yA(ȴA(�uA(^5A($�A'�A'K�A& �A%&�A$��A$M�A$$�A#�wA#dZA#33A"��A!�mA!��A!�A ��A �+A �A
=A�wA�DAt�A��A9XA�AVAz�A=qA1A|�AA�A~�A��A�HA^5AVA�A��A-A��AA{AA��A`BA�A
��A
ȴA
��A
�DA
A�A	�#A5?A�HAx�A �A33AI�A�A��At�AK�A A�@���@��@�V@�z�@�dZ@��T@�n�@�bN@�33@��@��/@�j@�  @�S�@�7L@��@�l�@���@�E�@��@�9X@�@��/@�|�@�"�@ް!@��@� �@��@�\)@�^5@թ�@�&�@Ԭ@�b@��#@�Q�@��@�{@͉7@̴9@�b@�;d@�|�@�&�@��@�Ĝ@�1@¸R@��#@�(�@��\@�hs@���@�Z@���@��\@���@���@���@�hs@�hs@�O�@���@��D@�A�@��;@��y@��+@�E�@�J@�V@�1'@��
@��y@�J@���@�A�@���@�|�@�"�@�
=@��@�@��@��w@�E�@���@���@��`@�ƨ@���@���@��@�O�@��9@�A�@��@�\)@��@��+@�^5@�$�@��T@���@�`B@�/@�z�@��@�-@�=q@�x�@��u@�K�@��@�~�@�J@���@��h@�O�@��D@�  @��@���@�"�@�v�@��^@��@�?}@���@���@��@�l�@�S�@���@�ff@���@�p�@�V@�bN@� �@��F@���@�l�@�K�@�;d@��@��R@�M�@���@���@���@�x�@�`B@��@��@��;@���@��7@�`B@��@��9@���@��u@�r�@�I�@� �@�  @��
@��@���@��@�t�@�dZ@�S�@�;d@�o@�
=@�@��@��H@��R@���@�~�@�^5@�J@���@�G�@���@���@��`@���@��/@��/@��/@���@���@��@�A�@+@}O�@|��@|(�@|(�@|9X@{��@{��@{S�@{o@{@z�@z�H@z��@z��@z��@z�!@z��@z�\@z^5@y�#@y�^@y��@x�`@xb@w+@vE�@u�-@u`B@u?}@u�@uV@t��@s@r�\@r^5@rJ@q�#@q�7@q7L@p��@pr�@pA�@p1'@ol�@n�@n�R@n�R@n�+@m�@l�@l�@k�m@k�
@kƨ@k��@kt�@kC�@ko@j��@j~�@j=q@i�^@i�@h�9@h�u@hbN@hA�@hb@g�@g�;@g\)@fv�@f@e@e�-@e�h@e�h@e�h@e�@e`B@d��@dj@dI�@d�@c�@bn�@b�@a�#@a�#@a��@a��@a��@ax�@a%@`��@`�@`1'@_��@_��@_l�@_l�@_\)@_K�@_�@_
=@^��@^�@^��@^V@]�T@]`B@]?}@]�@\�@\�/@\�/@\�j@\�@\z�@\Z@\I�@\�@[�m@[�@[C�@[o@Z~�@Y��@Y��@Y�7@Yx�@YX@YG�@Y�@X�`@XĜ@XbN@W\)@V�R@Vff@Up�@T�/@T�j@T9X@S�@R�@R��@R��@R�!@R��@R^5@RM�@R=q@R�@RJ@Q�@Q��@Q��@QX@QG�@Q�@P�u@PbN@PQ�@Pb@O\)@N�+@Nff@N5?@N{@M��@MO�@L�@L�j@Lj@L�@K�F@K��@K�@KdZ@K33@J�\@J=q@I�#@Ihs@H�`@H  @G��@G�@Gl�@G
=@F�+@F{@E@E�h@EO�@E?}@EV@D��@D�@D�@D�/@D�D@D9X@C�@C"�@C@B�@B�@B�@B��@B��@B�\@BM�@A�^@A&�@@�`@@A�@?��@?K�@?�@?
=@>��@=�@=�@=/@<�@;�m@;S�@;C�@;C�@;@:^5@:-@:J@9�^@9x�@97L@9�@8��@8��@8�9@7�@7l�@7K�@7;d@7
=@6�@6�+@6v�@6ff@6E�@5�@5`B@5V@4��@4�@4�@4��@4Z@3��@3�@3�@3S�@3C�@3o@2�@2��@2�!@2-@1�^@1x�@1X@1G�@17L@0��@0�9@0bN@0Q�@01'@/��@/\)@.��@.$�@-�T@-O�@-V@,��@,�@,�@,��@,��@,��@,�D@,�D@,z�@,z�@,Z@,I�@,I�@,�@+�
@+��@+�@+33@*��@*��@)�#@)hs@)%@(�`@(�9@(��@(�u@(r�@'�@'�@'l�@'�@&�y@&ȴ@&ȴ@&�R@&��@%�@%��@%�@$�D@$9X@$9X@$9X@$(�@$1@#��@#S�@#33@#33@#@"�!@"n�@"J@!�#@!��@!�7@!x�@!hs@!&�@!%@ �`@ �@ A�@�P@\)@K�@+@�y@�R@�+@E�@�@�@�-@�h@�@`B@�@�j@�D@Z@I�@1@ƨ@�@33@o@�H@��@��@�\@n�@-@��@��@�@�`@�`@��@�@�;@��@|�@\)@;d@�@�y@��@�+@ff@5?@�@@��@�@�@`B@�@�@z�@(�@��@�m@�m@ƨ@��@S�@o@��@�\@^5@�@J@J@�@�#@�7@7L@�@%@��@%@%@��@��@r�@�@�P@K�@
=@�@E�@�-@/@��@�/@��@�j@��@j@Z@9X@9X@(�@�@1@��@ƨ@��@dZ@
�@
�!@
��@
��@
�\@
n�@
n�@
n�@
M�@
-@	�@	�#@	��@	�^@	��@	�7@	X@	X@	G�@	7L@	%@��@bN@Q�@Q�@A�@A�@A�@A�@A�@1'@ �@�@K�@�@�R@�R@��@v�@v�@ff@5?@�@@��@p�@`B@O�@/@/@�@/@�@�@/@�@��@��@�D@j@Z@I�@I�@9X@(�@�@��@�@dZ@S�@33@o@�!@�\@n�@~�@n�@^5@M�@=q@=q@-@-@-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BG�BE�BG�BF�BF�BG�BD�BC�B@�B#�B\B,BM�BA�B#�BYB}�Bn�BjB\)B�+BhsB�oB�B��B��B��B��B�oBx�B�DB��B��B�\B�=B|�Bw�BVBC�B7LB:^B$�BDBbBPB�B��B�fB�wB�B�uB�{B�Bo�BgmBI�BP�BN�BL�BD�BE�B49B�BDBPB�BJB	7BB
��B
�)B
ŢB
��B
ɺB
�dB
��B
w�B
}�B
[#B
M�B
M�B
6FB
�B
�B
)�B
�B
 �B
�B
VB	��B	��B	��B
  B	��B	�sB	�5B	�)B	�B	��B	�B	��B	��B	��B	��B	�jB	��B	�FB	�B	�!B	��B	�bB	� B	�oB	�1B	~�B	s�B	bNB	N�B	B�B	D�B	M�B	6FB	.B	.B	D�B	?}B	33B	(�B	)�B	2-B	0!B	!�B	!�B	�B	B�sB�#B��B�B��B	B��B�B�`B��B�-B�?B��B�;B�HB�NB�HB�5B�)B�B�
B��B�dB�{B|�B�-B�LB�9B�!B�B��B��B��B��B��B��B��B�PB��B��B��B��B��B��B��B�{B�\B�Bv�Bx�B�B�+B�7B�B�B�Bx�Bt�Bz�Bu�Bx�Bt�Bl�B]/BS�BR�BW
B[#BaHBcTBVB_;BbNBbNBZBZB]/BS�BC�B;dB�B2-B7LB49B=qB=qB?}B8RBE�BG�BG�BD�BG�BF�BE�BC�B<jB2-B�BuB{B�B�B"�B-B+B(�B �BJB��B�B�B�BuBPB��BDB�B�B �B%�B#�B�BuB�B!�B"�B�B�B�B�BoB�B'�B#�B�B�BhBbB!�B&�B%�B#�B�BoB�B�B!�B'�B$�B"�B�B+B�B33B2-B+B#�B&�B!�B!�B.B49B7LB49B49B;dBA�BB�BB�BC�BB�B?}B?}B?}B@�B;dBB�BD�BB�B<jB=qBC�B>wB>wBA�BH�BL�BO�BO�BQ�BN�BE�BD�BD�BE�BYBW
BS�BP�BVB]/BffBhsBe`BhsBiyBl�Bn�Bo�Bt�Bs�Bs�Bs�Bs�Bq�BjBhsB]/BP�B�B�+B�1B��B��B��B��B��B��B��B��B�B��B��B��B�B�LB�RB�RB�XB�XB�dBB��BBǮBȴB��B��B�B�B�)B�/B�;B�BB�5B�/B�BB�ZB�sB�B�B�B�yB�sB�B�B��B	JB	PB	\B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	%�B	%�B	&�B	(�B	)�B	)�B	)�B	)�B	+B	,B	-B	-B	1'B	7LB	<jB	A�B	A�B	C�B	D�B	D�B	C�B	B�B	B�B	@�B	=qB	>wB	B�B	R�B	XB	\)B	\)B	[#B	]/B	^5B	aHB	cTB	dZB	e`B	e`B	e`B	e`B	ffB	ffB	ffB	ffB	ffB	jB	iyB	gmB	jB	o�B	u�B	{�B	�B	�B	�B	�B	� B	|�B	�7B	�JB	�PB	�VB	�VB	�\B	�hB	�hB	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�?B	�RB	�XB	�^B	�dB	�dB	�dB	�^B	�^B	��B	ÖB	ƨB	ƨB	ƨB	ƨB	ŢB	ĜB	ÖB	ĜB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�NB	�NB	�NB	�TB	�ZB	�NB	�ZB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�mB	�ZB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
  B
B
B
B
B
B
%B
%B
%B
B
B
B
%B
%B
%B
%B
DB
JB
DB
DB
DB
PB
VB
bB
bB
oB
hB
oB
oB
oB
oB
bB
bB
\B
uB
�B
�B
�B
�B
�B
�B
�B
uB
uB
uB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
 �B
�B
"�B
#�B
"�B
#�B
$�B
$�B
$�B
$�B
#�B
!�B
$�B
'�B
'�B
'�B
'�B
'�B
)�B
)�B
(�B
'�B
&�B
)�B
,B
,B
+B
)�B
(�B
'�B
-B
.B
-B
.B
-B
-B
-B
-B
,B
,B
/B
0!B
1'B
0!B
/B
0!B
0!B
1'B
1'B
/B
.B
/B
1'B
33B
2-B
49B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
7LB
7LB
7LB
7LB
7LB
7LB
6FB
6FB
6FB
7LB
6FB
5?B
6FB
49B
7LB
9XB
:^B
;dB
;dB
;dB
:^B
9XB
;dB
<jB
<jB
=qB
>wB
?}B
>wB
=qB
:^B
<jB
<jB
=qB
@�B
B�B
B�B
B�B
A�B
@�B
A�B
C�B
C�B
B�B
A�B
C�B
B�B
D�B
D�B
E�B
E�B
E�B
D�B
E�B
E�B
C�B
D�B
C�B
G�B
H�B
H�B
G�B
H�B
H�B
H�B
I�B
K�B
J�B
K�B
K�B
K�B
J�B
J�B
L�B
L�B
M�B
L�B
L�B
M�B
L�B
N�B
N�B
O�B
O�B
O�B
O�B
N�B
O�B
N�B
M�B
Q�B
R�B
Q�B
O�B
N�B
R�B
S�B
S�B
S�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
VB
T�B
VB
W
B
W
B
XB
YB
YB
YB
XB
XB
YB
YB
ZB
[#B
[#B
\)B
\)B
\)B
[#B
[#B
[#B
]/B
^5B
^5B
^5B
^5B
]/B
\)B
[#B
[#B
\)B
^5B
^5B
^5B
]/B
]/B
_;B
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
cTB
cTB
cTB
cTB
bNB
dZB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
gmB
ffB
ffB
gmB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
gmB
ffB
ffB
gmB
jB
k�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
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
o�B
o�B
n�B
n�B
m�B
o�B
o�B
o�B
o�B
n�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BG�BE�BG�BF�BF�BG�BEBDgBB�B)�B�B2�BS�BG�B*�B]/B�Br|Bn/BaHB�=BmwB�{B��B�mB�$B�B�'B��B|�B�6B��B��B� B��B~wBx�BX�BFtB9�B<jB'�B�B&BBBYB�0B��B�'B��B�B��B�{Bq�Bi�BL~BR BO�BM�BE�BF%B5�B�B"B�BmB6B	�B�B
��B
�OB
ȀB
��B
ʌB
��B
��B
z�B
�4B
^�B
P�B
O�B
9$B
"hB
 �B
+QB
 'B
!�B
�B
�B	��B
 4B	�lB
 iB	��B	��B	ߊB	�~B	�kB	�VB	�B	��B	ѝB	ˬB	ªB	��B	�B	��B	�B	��B	��B	�oB	��B	�B	�7B	�4B	uB	d@B	QNB	EB	FtB	N�B	8�B	0oB	/�B	EB	@B	4�B	*B	+6B	2|B	0�B	"�B	"NB	kB	�B�kB�5B��B�MB�}B	GB�]B�B�B��B�B��B��B�pB�|B�B�bBޞBܒBچB�YB̈́B��B�_B�OB��B��B��B��B��B��B�B��B��B��B�HB��B��B�B�=B�#B��B�B�	B��B��B��B�9BxlBz*B��B��B��B��B��B�uBy�Bu�B{JBv�By>ButBmwB^�BU�BT�BXyB\CBa�Bc�BW?B_�Bb�Bb�BZ�BZ�B]~BT�BEB=<B!�B3�B8�B5�B>]B>wB@iB9�BF%BG�BG�BEBG�BF�BFBC�B=B3MB�BgBSB=B�B#�B-wB+�B)_B!HB�B�qBQB�B�BaB�B��B~BIB�B!�B&fB$ZBpB�BxB"NB#TBVBeBxB�B�B�B(XB$tB�B�B�B�B"hB'mB&fB$ZB�B�B�B �B"�B(sB%�B#�B�B	�B
B3MB2�B+�B%B'�B# B"�B.�B4�B7�B4�B4�B;�BA�BB�BB�BC�BB�B?�B@ B?�B@�B<6BB�BEBB�B=VB>(BC�B?cB?.BB[BIRBM6BP.BPHBR BOBBF�BEmBE�BF�BYeBWsBT�BQ�BV�B]�Bf�Bh�Be�Bh�BjBl�Bo Bo�Bt�BtBtBtBtBq�BkQBi*B^�BSuB�oB��B�B��B�	B�B��B�B�B�pB�:B�"B�DB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�<B�\B�9B�_B�]B�~B�VB�\B�jBݘB��B��B�B�B�B��B��B�B�6B�B��B	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	%B	%�B	%�B	%�B	'B	)B	*0B	*B	*B	*0B	+6B	,=B	-CB	-]B	1vB	7�B	<�B	A�B	A�B	C�B	D�B	D�B	C�B	B�B	B�B	@�B	=�B	?.B	C-B	S&B	X+B	\)B	\]B	[qB	]IB	^jB	abB	cnB	dtB	e�B	ezB	e`B	e�B	f�B	f�B	f�B	f�B	f�B	j�B	i�B	g�B	j�B	p!B	v+B	|6B	� B	�GB	�GB	�'B	�OB	}�B	�RB	�~B	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�@B	�B	�)B	�)B	�=B	�)B	�)B	�)B	�/B	�IB	�5B	�iB	�vB	�tB	��B	�rB	��B	�B	�B	�B	��B	��B	��B	ðB	ƨB	��B	��B	ƨB	żB	��B	��B	��B	��B	��B	�B	�	B	�B	�B	��B	�2B	��B	�,B	�,B	�FB	�B	�B	�9B	�+B	�1B	�=B	�CB	�)B	�WB	�=B	�CB	�]B	�CB	�CB	�]B	�xB	�dB	�\B	�|B	�|B	�NB	�hB	�hB	�hB	�hB	�hB	�nB	�B	�B	�B	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�	B	�B	�B	�B	�>B	�DB
  B
 B
 B	�HB
 B
;B
AB
AB
GB
MB
%B
?B
YB
9B
{B
9B
tB
tB
tB
�B
^B
dB
^B
xB
xB
�B
pB
}B
�B
�B
�B
oB
oB
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#B
B
"�B
!�B
 �B
�B
#B
#�B
#B
$B
$�B
%B
$�B
$�B
$B
"B
%B
'�B
(
B
($B
(
B
(
B
)�B
*B
)B
(>B
'B
*B
,B
,B
+B
*B
)*B
(>B
-B
.B
-)B
./B
-)B
-)B
-)B
-)B
,=B
,WB
/OB
0;B
1'B
0UB
/5B
0UB
0;B
1[B
1AB
/iB
.IB
/�B
1[B
3MB
2aB
4TB
5tB
6`B
7LB
7fB
7LB
8RB
8RB
7fB
7fB
7LB
7�B
7fB
7LB
6`B
6`B
6zB
7�B
6`B
5�B
6zB
4�B
7�B
9rB
:xB
;B
;dB
;B
:xB
9�B
;�B
<�B
<�B
=�B
>�B
?�B
>�B
=�B
:�B
<�B
<�B
=�B
@�B
B�B
B�B
B�B
A�B
@�B
A�B
C�B
C�B
B�B
A�B
C�B
B�B
D�B
D�B
E�B
E�B
E�B
D�B
E�B
E�B
C�B
D�B
C�B
G�B
H�B
H�B
G�B
H�B
H�B
H�B
I�B
K�B
J�B
K�B
K�B
K�B
J�B
J�B
L�B
L�B
NB
L�B
L�B
NB
L�B
N�B
OB
O�B
PB
PB
O�B
OB
O�B
OB
NB
RB
R�B
RB
PB
OBB
SB
TB
T,B
TB
UB
TB
TB
U2B
UB
UB
UB
V9B
V9B
W$B
W
B
VB
U2B
VB
W?B
W?B
XEB
Y1B
YB
Y1B
X+B
XEB
YKB
YKB
Z7B
[WB
[=B
\)B
\CB
\CB
[WB
[=B
[WB
]IB
^OB
^5B
^5B
^5B
]IB
\]B
[WB
[WB
\xB
^OB
^OB
^jB
]dB
]dB
_pB
bhB
cTB
cTB
cnB
cnB
c�B
c�B
dZB
dZB
dZB
dtB
dZB
cnB
c�B
c�B
c�B
b�B
dtB
f�B
ffB
ffB
f�B
f�B
f�B
f�B
ezB
f�B
g�B
gmB
gmB
g�B
g�B
g�B
hsB
gmB
g�B
f�B
f�B
g�B
iyB
i�B
iyB
iyB
iyB
i�B
i�B
h�B
g�B
f�B
f�B
g�B
jB
k�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
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
o�B
o�B
n�B
n�B
m�B
o�B
o�B
o�B
o�B
n�B
q�B
q�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807020036522018070200365220180702003652201807020200172018070202001720180702020017201807030027492018070300274920180703002749  JA  ARFMdecpA19c                                                                20180628093533  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180628003535  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180628003538  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180628003538  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180628003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180628003539  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180628003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180628003539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180628003539  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180628003539                      G�O�G�O�G�O�                JA  ARUP                                                                        20180628005608                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180628153743  CV  JULD            G�O�G�O�F�m�                JM  ARCAJMQC2.0                                                                 20180701153652  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180701153652  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180701170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180702152749  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                