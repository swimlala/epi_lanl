CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-07-05T09:00:36Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
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
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܐ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20210705090036  20210705090036  4903175 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               cA   AO  7231                            2B  A   NAVIS_A                         0968                            170425                          863 @ف��%5
1   @ف��e�@13�E����c�r� Ĝ1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         cA   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�zC3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D��)D��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�l�A�n�A�n�A�n�A�hsA�dZA�ZA�^5A�\)A��A�1'A�-A��A�oA��mA���AЙ�AБhAЋDAЅAЁA�x�A�t�A�r�A�n�A�l�A�jA�ffA�XA�/A���AϑhA�A�A� �A���A�^5A�A��A��`A��
A���AͼjAͰ!Aͩ�A͗�A̓A�ffA�O�A��A̩�A�A���Aʉ7A���A��A��HAǣ�A�bNA�VA�$�Aƣ�A�hsA�&�AŰ!AŇ+A��AĶFA�bNA�Að!A�^5A�(�A¬A�(�A�1'A�\)A��/A�  A��!A�5?A���A�t�A�z�A��A��yA��A���A���A��A�A�A�ĜA���A��wA���A��PA�33A��A�v�A���A���A�^5A��^A�
=A�O�A��A��RA�1A��9A���A�VA���A�XA��A��A�;dA��yA�=qA~��A{�
Ax��Ax�+Au�PApĜAm�7AlȴAj��Agx�AdE�A`�yA^�uA\��AZ-AV�AT��AS��AR�jAQ+AN9XAI�7AF�RAC�TAA�A>A<n�A:M�A8ZA7�A5dZA0�A/t�A. �A,�A,A,��A*�`A)VA(VA'|�A&�A%��A#��A"ȴA!dZA E�A�^AC�A=qAO�A�DA1'A�AƨA��A�hAl�A+A~�A�A��A�AffA=qA(�Ap�A�9A�A�A��A��A�A��A��AĜA�#A
=A
�!A
ffA	��AVA��A�A^5A�
AhsA�9A�A5?A �`@��w@���@�&�@���@�S�@���@���@�bN@�+@�@� �@��@��#@��@�K�@�x�@�7L@�I�@�K�@�!@��#@�&�@���@�E�@��/@�A�@��m@�\)@��@އ+@��@�O�@ܼj@۝�@�@�ȴ@ԋD@�b@Ұ!@��@�?}@���@Ѻ^@щ7@�  @�E�@Ͳ-@̋D@̬@�bN@��T@��H@�%@Ƨ�@ǝ�@�S�@��;@ư!@��@�l�@+@��P@�+@���@��R@�5?@�hs@��D@���@��@�1'@�
=@�ȴ@�=q@�&�@���@�z�@�  @���@�dZ@��\@��7@�{@��@�p�@�hs@�G�@��@�9X@�t�@�K�@�"�@��R@�n�@���@��7@�hs@�V@��j@��@�9X@�b@���@�\)@�S�@�+@��@��+@�M�@���@�x�@�%@��9@���@�r�@�  @��
@���@�1'@�1'@�Q�@�Z@�t�@���@�~�@�=q@��R@�ȴ@�ȴ@�V@�=q@�v�@���@��+@�n�@�M�@��@� �@��m@�dZ@��P@�ƨ@�"�@�@���@���@��\@�{@���@��#@�=q@�$�@��-@��-@��h@�?}@�z�@�bN@�1'@�(�@�  @�1@�b@�b@��m@���@���@�;d@�o@�\)@�ff@�`B@���@�p�@�G�@�hs@�/@���@��D@�1'@��m@��m@�ƨ@��P@�t�@�+@�@���@�=q@���@��-@�?}@���@�r�@� �@��;@�|�@��y@�^5@��@���@�7L@���@��j@�z�@�(�@���@�|�@��@���@�M�@�@���@�G�@��@���@��`@���@��@�Q�@���@�C�@�@���@��@�@���@�x�@�p�@��@��/@��9@��u@�Q�@�  @�  @�  @��w@�;d@��@�ȴ@���@�~�@���@��^@���@�O�@��@���@���@�Z@�b@��m@�l�@�;d@�@��!@�~�@�M�@�J@���@�X@��@��u@�b@��
@��P@�S�@�C�@�"�@��@��@��+@�v�@�n�@�V@�M�@�-@�@���@�O�@�&�@���@���@��`@��j@�j@��@���@�l�@�C�@�"�@�
=@���@�n�@�J@���@�p�@���@��u@�9X@�;@��@+@~�+@}�@}O�@}V@|�j@|z�@{��@{t�@z��@y�#@y�7@y�^@y��@y&�@xQ�@w��@w�P@w;d@w
=@v��@v��@v5?@v$�@v$�@v@u��@uO�@u�@t�@t�@t�@s��@st�@s"�@r��@r=q@q��@qx�@p�@o|�@n��@n�+@n$�@m��@m�@l�@l�D@lj@l(�@kƨ@k�@kC�@ko@j�H@j�!@j^5@i��@h�9@hA�@g��@g�@g�P@g\)@g+@f��@f��@fv�@fv�@fv�@fV@e�@e@e`B@eV@d�D@dZ@d9X@d�@cƨ@c��@c�@ct�@cC�@b�\@b�@a�^@aX@a�@`�9@_�;@_��@_+@_
=@^ȴ@^��@^$�@]?}@\j@[��@[��@[C�@[o@[@Z~�@Yhs@XbN@X  @W�@Wl�@Vȴ@V��@V��@V��@V��@Vv�@V$�@V@U�T@U�h@UO�@T��@T�j@TI�@S��@S��@SS�@SC�@So@R�@R��@R�\@R=q@Q��@Q�7@P��@P�@P1'@P  @O\)@N�@N��@NV@M�-@M�h@M�@L��@L�@Lz�@L1@Kt�@K"�@K@J�!@J~�@J�@I�@I��@I��@I��@IX@I%@H�9@Hr�@HQ�@H1'@H �@G�w@G+@Fȴ@F�+@Fff@FV@F$�@F{@E��@Ep�@E�@D�@D�@C�
@Ct�@Co@B��@B��@B�\@B~�@B^5@B=q@B�@A��@A�^@@��@@�9@@r�@@1'@?�;@?|�@?+@>�@>ff@=��@=p�@=?}@=/@<��@<�j@<�@<�D@<j@;�m@;�F@;t�@;o@:�H@:��@:�!@:~�@:^5@:�@9��@9�#@9��@9�7@9G�@8�`@8��@8��@7��@7\)@7;d@7�@6�y@6��@6��@6ff@65?@6{@6@5�@5�T@5�-@5�h@4�/@4I�@41@3��@3�m@3�
@3ƨ@3t�@2�@2�!@2=q@1��@1��@1��@1��@1��@0�@/�@/��@/��@.�y@.v�@-�@-�-@-�@-O�@-?}@-/@-�@,�/@,�@,j@,(�@+�
@+�@+33@*�H@*��@*�!@*��@*�\@*n�@*n�@*^5@*M�@*M�@*=q@*J@)��@)�7@)x�@)x�@)G�@)�@(Ĝ@(bN@(b@'�;@'|�@'�@&ȴ@&v�@&E�@&@%��@%p�@%/@$�@$�@$�D@$j@$Z@$I�@$9X@$(�@#�
@#��@#S�@#33@"��@"�\@"n�@"n�@"^5@"-@"J@!��@!&�@ ��@ �9@ �u@ bN@   @�;@��@�R@�@�@�T@@�h@�@O�@/@�/@�D@(�@�F@�@dZ@33@��@�!@��@~�@M�@-@�#@��@7L@�@%@��@�9@bN@1'@��@�P@K�@�@�y@ȴ@�+@V@5?@��@�h@�@p�@`B@/@�@��@z�@1@ƨ@�F@�F@�F@dZ@33@@�H@��@��@�\@^5@=q@�#@��@�7@x�@X@X@7L@%@��@��@�9@�u@bN@bN@Q�@A�@ �@�w@\)@�@�y@��@�@�-@�h@p�@O�@/@V@��@�/@��@j@I�@I�@9X@�m@�
@t�@"�@@
��@
��@
�!@
��@
~�@
n�@
^5@
=q@
=q@
-@
�@
J@	�@	�@	��@	��@	��@	hs@	G�@	7L@	�@��@��@�9@�u@r�@bN@ �@  @�@��@�w@�@|�@\)@�@
=@�y@�R@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�jA�l�A�n�A�n�A�n�A�hsA�dZA�ZA�^5A�\)A��A�1'A�-A��A�oA��mA���AЙ�AБhAЋDAЅAЁA�x�A�t�A�r�A�n�A�l�A�jA�ffA�XA�/A���AϑhA�A�A� �A���A�^5A�A��A��`A��
A���AͼjAͰ!Aͩ�A͗�A̓A�ffA�O�A��A̩�A�A���Aʉ7A���A��A��HAǣ�A�bNA�VA�$�Aƣ�A�hsA�&�AŰ!AŇ+A��AĶFA�bNA�Að!A�^5A�(�A¬A�(�A�1'A�\)A��/A�  A��!A�5?A���A�t�A�z�A��A��yA��A���A���A��A�A�A�ĜA���A��wA���A��PA�33A��A�v�A���A���A�^5A��^A�
=A�O�A��A��RA�1A��9A���A�VA���A�XA��A��A�;dA��yA�=qA~��A{�
Ax��Ax�+Au�PApĜAm�7AlȴAj��Agx�AdE�A`�yA^�uA\��AZ-AV�AT��AS��AR�jAQ+AN9XAI�7AF�RAC�TAA�A>A<n�A:M�A8ZA7�A5dZA0�A/t�A. �A,�A,A,��A*�`A)VA(VA'|�A&�A%��A#��A"ȴA!dZA E�A�^AC�A=qAO�A�DA1'A�AƨA��A�hAl�A+A~�A�A��A�AffA=qA(�Ap�A�9A�A�A��A��A�A��A��AĜA�#A
=A
�!A
ffA	��AVA��A�A^5A�
AhsA�9A�A5?A �`@��w@���@�&�@���@�S�@���@���@�bN@�+@�@� �@��@��#@��@�K�@�x�@�7L@�I�@�K�@�!@��#@�&�@���@�E�@��/@�A�@��m@�\)@��@އ+@��@�O�@ܼj@۝�@�@�ȴ@ԋD@�b@Ұ!@��@�?}@���@Ѻ^@щ7@�  @�E�@Ͳ-@̋D@̬@�bN@��T@��H@�%@Ƨ�@ǝ�@�S�@��;@ư!@��@�l�@+@��P@�+@���@��R@�5?@�hs@��D@���@��@�1'@�
=@�ȴ@�=q@�&�@���@�z�@�  @���@�dZ@��\@��7@�{@��@�p�@�hs@�G�@��@�9X@�t�@�K�@�"�@��R@�n�@���@��7@�hs@�V@��j@��@�9X@�b@���@�\)@�S�@�+@��@��+@�M�@���@�x�@�%@��9@���@�r�@�  @��
@���@�1'@�1'@�Q�@�Z@�t�@���@�~�@�=q@��R@�ȴ@�ȴ@�V@�=q@�v�@���@��+@�n�@�M�@��@� �@��m@�dZ@��P@�ƨ@�"�@�@���@���@��\@�{@���@��#@�=q@�$�@��-@��-@��h@�?}@�z�@�bN@�1'@�(�@�  @�1@�b@�b@��m@���@���@�;d@�o@�\)@�ff@�`B@���@�p�@�G�@�hs@�/@���@��D@�1'@��m@��m@�ƨ@��P@�t�@�+@�@���@�=q@���@��-@�?}@���@�r�@� �@��;@�|�@��y@�^5@��@���@�7L@���@��j@�z�@�(�@���@�|�@��@���@�M�@�@���@�G�@��@���@��`@���@��@�Q�@���@�C�@�@���@��@�@���@�x�@�p�@��@��/@��9@��u@�Q�@�  @�  @�  @��w@�;d@��@�ȴ@���@�~�@���@��^@���@�O�@��@���@���@�Z@�b@��m@�l�@�;d@�@��!@�~�@�M�@�J@���@�X@��@��u@�b@��
@��P@�S�@�C�@�"�@��@��@��+@�v�@�n�@�V@�M�@�-@�@���@�O�@�&�@���@���@��`@��j@�j@��@���@�l�@�C�@�"�@�
=@���@�n�@�J@���@�p�@���@��u@�9X@�;@��@+@~�+@}�@}O�@}V@|�j@|z�@{��@{t�@z��@y�#@y�7@y�^@y��@y&�@xQ�@w��@w�P@w;d@w
=@v��@v��@v5?@v$�@v$�@v@u��@uO�@u�@t�@t�@t�@s��@st�@s"�@r��@r=q@q��@qx�@p�@o|�@n��@n�+@n$�@m��@m�@l�@l�D@lj@l(�@kƨ@k�@kC�@ko@j�H@j�!@j^5@i��@h�9@hA�@g��@g�@g�P@g\)@g+@f��@f��@fv�@fv�@fv�@fV@e�@e@e`B@eV@d�D@dZ@d9X@d�@cƨ@c��@c�@ct�@cC�@b�\@b�@a�^@aX@a�@`�9@_�;@_��@_+@_
=@^ȴ@^��@^$�@]?}@\j@[��@[��@[C�@[o@[@Z~�@Yhs@XbN@X  @W�@Wl�@Vȴ@V��@V��@V��@V��@Vv�@V$�@V@U�T@U�h@UO�@T��@T�j@TI�@S��@S��@SS�@SC�@So@R�@R��@R�\@R=q@Q��@Q�7@P��@P�@P1'@P  @O\)@N�@N��@NV@M�-@M�h@M�@L��@L�@Lz�@L1@Kt�@K"�@K@J�!@J~�@J�@I�@I��@I��@I��@IX@I%@H�9@Hr�@HQ�@H1'@H �@G�w@G+@Fȴ@F�+@Fff@FV@F$�@F{@E��@Ep�@E�@D�@D�@C�
@Ct�@Co@B��@B��@B�\@B~�@B^5@B=q@B�@A��@A�^@@��@@�9@@r�@@1'@?�;@?|�@?+@>�@>ff@=��@=p�@=?}@=/@<��@<�j@<�@<�D@<j@;�m@;�F@;t�@;o@:�H@:��@:�!@:~�@:^5@:�@9��@9�#@9��@9�7@9G�@8�`@8��@8��@7��@7\)@7;d@7�@6�y@6��@6��@6ff@65?@6{@6@5�@5�T@5�-@5�h@4�/@4I�@41@3��@3�m@3�
@3ƨ@3t�@2�@2�!@2=q@1��@1��@1��@1��@1��@0�@/�@/��@/��@.�y@.v�@-�@-�-@-�@-O�@-?}@-/@-�@,�/@,�@,j@,(�@+�
@+�@+33@*�H@*��@*�!@*��@*�\@*n�@*n�@*^5@*M�@*M�@*=q@*J@)��@)�7@)x�@)x�@)G�@)�@(Ĝ@(bN@(b@'�;@'|�@'�@&ȴ@&v�@&E�@&@%��@%p�@%/@$�@$�@$�D@$j@$Z@$I�@$9X@$(�@#�
@#��@#S�@#33@"��@"�\@"n�@"n�@"^5@"-@"J@!��@!&�@ ��@ �9@ �u@ bN@   @�;@��@�R@�@�@�T@@�h@�@O�@/@�/@�D@(�@�F@�@dZ@33@��@�!@��@~�@M�@-@�#@��@7L@�@%@��@�9@bN@1'@��@�P@K�@�@�y@ȴ@�+@V@5?@��@�h@�@p�@`B@/@�@��@z�@1@ƨ@�F@�F@�F@dZ@33@@�H@��@��@�\@^5@=q@�#@��@�7@x�@X@X@7L@%@��@��@�9@�u@bN@bN@Q�@A�@ �@�w@\)@�@�y@��@�@�-@�h@p�@O�@/@V@��@�/@��@j@I�@I�@9X@�m@�
@t�@"�@@
��@
��@
�!@
��@
~�@
n�@
^5@
=q@
=q@
-@
�@
J@	�@	�@	��@	��@	��@	hs@	G�@	7L@	�@��@��@�9@�u@r�@bN@ �@  @�@��@�w@�@|�@\)@�@
=@�y@�R@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�XA�`BA�bNA�bNA�ffA�dZA�ffA�ffA�dZA�hsA�dZA�hsA�jA�ffA�hsA�ffA�jA�hsA�ffA�hsA�jA�l�A�l�A�l�A�n�A�n�A�n�A�l�A�l�A�jA�ffA�^5A�ZA�VA�O�A�M�A�M�A�M�A�O�A�M�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�O�A�M�A�I�A�A�A�t�A㙚A㛦A㟾A��A�RA�A���A���A��
A���A�JA��A�7LA�?}A�Q�A�l�A�A��A�^A��A���A�&�A�A�^A��#A��A�dZA�A��#A�$�A�7LA�`BA�\A��A�%A�bNA�n�A�t�A�|�A蝲A�FA��A�JA�-A�%A�DA���A��A�JA���A�{A��A��A�+A�5?A�/A��/A��
A���A���A훦A�\)A�1'A�/A��A�A��`A�A�A웦A�+A�\)A�;dA�/A�"�A�%A��A���A�A�!A��A�uA�A�A�|�A�p�A�ffA�K�A�E�A�9XA�+A��A�oA�VA�VA�
=A�
=A���A��A��A��TA��mA�%A�%A���A�%A�A���A��A��/A���A�ȴA���A�wA���A�ƨA�A�A���A�A�A���A���A�wA�^A�FA�-A�!A�A�jA�wA�A���A���A��;A��;A��#A���A���A�!AꛦA�uAꕁAꕁA�\A�DA�A�|�A�z�A�z�A�x�A�v�A�t�A�p�A�l�A�hsA�jA�r�A�A�|�A�~�A�PA�A�z�A�v�A�z�A�|�A�~�A�A�A�A�\AꙚAꗍAꗍAꕁA�uA�hA�\A�uAꙚAꝲAꟾA��A��A�!A�FA�RA�wA���A���A���A�ĜA�wA�wA�A���A���A��#A��/A��HA��HA��A��A��A��TA��yA��HA��
A��A��A�A�
=A��A��A��A�{A��A�bA�bA�oA�{A��A� �A�-A�5?A�9XA�?}A�M�A�O�A�Q�A�VA�VA�XA�\)A�dZA�jA�n�A�r�A�A�DA�hA땁A뙚A뛦A띲A��A��A�A�-A�RA�jA�A�ƨA���A���A���A���A��
A��/A��;A��HA��TA��mA��A��A���A�A�
=A�bA�bA�oA��A��A�$�A�/A�33A�7LA�=qA�C�A�E�A�E�A�E�A�Q�A�S�A�ZA�ZA�ZA�^5A�ffA�hsA�jA�hsA�dZA�\)A�\)A�^5A�dZA�jA�jA�l�A�n�A�p�A�r�A�x�A�z�A�A�+A�7A�7A�7A�7A�DA�PA�hA앁A엍A웦A웦A쟾A��A��A�A�-A�-A�!A�FA�9A�9A�^A�^A�^A�wA�wA���A�wA�wA�wA���A���A���A���A���A�A�ĜA�ȴA�ȴA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��A��#A��/A��;A��;A��HA��TA��`A��`A��mA��yA��mA��mA��mA��mA��mA��mA��mA��mA��mA��mA��yA��yA��A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�  A�  A�  A���A�  A�  A�  A�  A�  A�  A�  A�  A�A�A�A�A�A�A�A�  A�A�  A�A�A�A�A�%A�%A�%A�%A�%A�%A�%A�%A�%A�%A�%A�A�A�%A�1A�1A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�JA�VA�VA�bA�bA�bA�oA�bA�oA�bA�oA�oA�oA�oA�oA�oA�{A�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�&�A�&�A�$�A�&�A�&�A�&�A�(�A�&�A�(�A�(�A�(�A�+A�(�A�+A�(�A�+A�+A�-A�-A�-A�-A�-A�-A�-A�/A�1'A�1'A�1'A�1'A�1'A�33A�1'A�1'A�1'A�1'A�33A�33A�1'A�1'A�33A�33A�33A�33A�33A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�5?A�7LA�7LA�7LA�7LA�5?A�7LA�7LA�7LA�7LA�7LA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�=qA�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�A�A�A�A�A�A�A�A�A�A�C�A�A�A�A�A�C�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�I�A�K�A�K�A�I�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�XA�VA�VA�VA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�dZ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  A�K�A�XA�`BA�bNA�bNA�ffA�dZA�ffA�ffA�dZA�hsA�dZA�hsA�jA�ffA�hsA�ffA�jA�hsA�ffA�hsA�jA�l�A�l�A�l�A�n�A�n�A�n�A�l�A�l�A�jA�ffA�^5A�ZA�VA�O�A�M�A�M�A�M�A�O�A�M�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�O�A�M�A�I�A�A�A�t�A㙚A㛦A㟾A��A�RA�A���A���A��
A���A�JA��A�7LA�?}A�Q�A�l�A�A��A�^A��A���A�&�A�A�^A��#A��A�dZA�A��#A�$�A�7LA�`BA�\A��A�%A�bNA�n�A�t�A�|�A蝲A�FA��A�JA�-A�%A�DA���A��A�JA���A�{A��A��A�+A�5?A�/A��/A��
A���A���A훦A�\)A�1'A�/A��A�A��`A�A�A웦A�+A�\)A�;dA�/A�"�A�%A��A���A�A�!A��A�uA�A�A�|�A�p�A�ffA�K�A�E�A�9XA�+A��A�oA�VA�VA�
=A�
=A���A��A��A��TA��mA�%A�%A���A�%A�A���A��A��/A���A�ȴA���A�wA���A�ƨA�A�A���A�A�A���A���A�wA�^A�FA�-A�!A�A�jA�wA�A���A���A��;A��;A��#A���A���A�!AꛦA�uAꕁAꕁA�\A�DA�A�|�A�z�A�z�A�x�A�v�A�t�A�p�A�l�A�hsA�jA�r�A�A�|�A�~�A�PA�A�z�A�v�A�z�A�|�A�~�A�A�A�A�\AꙚAꗍAꗍAꕁA�uA�hA�\A�uAꙚAꝲAꟾA��A��A�!A�FA�RA�wA���A���A���A�ĜA�wA�wA�A���A���A��#A��/A��HA��HA��A��A��A��TA��yA��HA��
A��A��A�A�
=A��A��A��A�{A��A�bA�bA�oA�{A��A� �A�-A�5?A�9XA�?}A�M�A�O�A�Q�A�VA�VA�XA�\)A�dZA�jA�n�A�r�A�A�DA�hA땁A뙚A뛦A띲A��A��A�A�-A�RA�jA�A�ƨA���A���A���A���A��
A��/A��;A��HA��TA��mA��A��A���A�A�
=A�bA�bA�oA��A��A�$�A�/A�33A�7LA�=qA�C�A�E�A�E�A�E�A�Q�A�S�A�ZA�ZA�ZA�^5A�ffA�hsA�jA�hsA�dZA�\)A�\)A�^5A�dZA�jA�jA�l�A�n�A�p�A�r�A�x�A�z�A�A�+A�7A�7A�7A�7A�DA�PA�hA앁A엍A웦A웦A쟾A��A��A�A�-A�-A�!A�FA�9A�9A�^A�^A�^A�wA�wA���A�wA�wA�wA���A���A���A���A���A�A�ĜA�ȴA�ȴA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��A��#A��/A��;A��;A��HA��TA��`A��`A��mA��yA��mA��mA��mA��mA��mA��mA��mA��mA��mA��mA��yA��yA��A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�  A�  A�  A���A�  A�  A�  A�  A�  A�  A�  A�  A�A�A�A�A�A�A�A�  A�A�  A�A�A�A�A�%A�%A�%A�%A�%A�%A�%A�%A�%A�%A�%A�A�A�%A�1A�1A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�JA�VA�VA�bA�bA�bA�oA�bA�oA�bA�oA�oA�oA�oA�oA�oA�{A�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�&�A�&�A�$�A�&�A�&�A�&�A�(�A�&�A�(�A�(�A�(�A�+A�(�A�+A�(�A�+A�+A�-A�-A�-A�-A�-A�-A�-A�/A�1'A�1'A�1'A�1'A�1'A�33A�1'A�1'A�1'A�1'A�33A�33A�1'A�1'A�33A�33A�33A�33A�33A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�5?A�7LA�7LA�7LA�7LA�5?A�7LA�7LA�7LA�7LA�7LA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�=qA�?}A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�A�A�A�A�A�A�A�A�A�A�C�A�A�A�A�A�C�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�G�A�I�A�K�A�K�A�I�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�XA�VA�VA�VA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�dZ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210705090036                              AO  ARCAADJP                                                                    20210705090036    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210705090036  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210705090036  QCF$                G�O�G�O�G�O�8000            