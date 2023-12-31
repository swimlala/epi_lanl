CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:04:59Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190459  20181005190459  5904950 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6429                            2B  A   APEX                            7464                            062512                          846 @כ�=�@O1   @כ����@3��-V�c��7Kƨ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @���@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C�fC  C  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D  D�fD  D� D  D�fDfD� D  D� D  D�fD	fD	�fD
  D
�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;y�D<  D<� D=fD=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DVfDV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dw�fDy��D�R=D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�\)@�@�A�HA>�HA^�HA~�HA�p�A���A�p�A�p�A�p�Aޣ�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBWQ�B_�RBg�RBo�RBw�RB�RB��)B�\B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B���B���B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)C�C�C�C�C	�C�C�C�C�zC�C�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�zCy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��=C��
C��C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��DuD��D��D��D{�D��D��D�D{�D��D{�D��D��D	�D	��D	��D
��D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��DuD��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�DuD��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%��D%��D&{�D&��D'{�D'��D({�D(�D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D1�D1{�D1��D2��D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:�D;uD;��D<{�D=�D={�D=��D>{�D>��D?uD?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE��DE��DF{�DF��DG{�DG��DH{�DH��DI��DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT�DU{�DV�DV{�DV��DW{�DW��DX{�DX�DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dn�Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr�Ds{�Ds��Dt{�Dt��Du��Du��Dv{�Dv��Dw{�Dw��Dy�3D�P D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�-A�1'A�7LA�?}A�E�A�G�A�C�A�C�A�?}A�?}A�?}A�A�A�G�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�K�A�A�A�?}A�33A���Aɡ�A���A�JA��TA���A�jA��#AƑhA�-Aơ�A�\)A�1A�oAƏ\A�+A��A�33A�ZA�l�A�+AĲ-A×�A�`BA���A�%A�A�A��A�9XA�A���A�"�A�5?A�jA�VA�K�A��A�A��9A��A��A��mA�oA���A���A��A��A� �A� �As�hAo�AiO�Ad�!Aa33A_&�A]XAYG�AV=qAT��ANz�AH�/AF��AF-AE33AC�AA7LA@��A@=qA>$�A<��A<$�A;/A9ƨA6ĜA5&�A4E�A2�HA2ĜA2��A1?}A/l�A.��A.v�A.n�A.~�A.JA+�TA+33A)��A'�-A&�\A&VA%�#A!t�A�A��A�RA��AVA�mA��A��A�wAO�A��AS�A�
A�uA�AƨA�AhsAȴAhsAĜA��A�\A�
AG�A%A	A��A�;AO�AS�A�jAM�A{A��A�9A��AS�Av�Ax�A ��A   @���@�v�@��T@�ƨ@���@�
=@��@���@���@�&�@�A�@��@���@�x�@�%@�dZ@�n�@�E�@�@���@�|�@ޗ�@���@�33@���@��@�K�@���@�1'@ӝ�@Ұ!@�v�@љ�@�7L@��`@�b@�V@͑h@�V@�%@�bN@˝�@�o@���@��@ɑh@�V@�Ĝ@�z�@�(�@��@Ǿw@�t�@�\)@�33@�"�@�@�ȴ@ư!@Ƈ+@�ff@Ų-@�V@� �@å�@�t�@�;d@���@¸R@+@�{@�@��-@���@�?}@�Ĝ@��@���@�j@�1@�\)@�@�^5@���@�@��u@�l�@�C�@�
=@�=q@���@��@���@�l�@�+@�~�@�7L@���@�9X@��F@��m@��F@�|�@��@�n�@�hs@�+@�ff@�v�@���@�/@�r�@�  @�\)@�o@�V@���@��!@�5?@�J@���@���@��-@�@�@���@���@���@�`B@�bN@�1'@�1'@�(�@��
@���@�|�@�t�@�t�@�l�@��@��!@�E�@�@��#@��^@��@�&�@��D@�I�@� �@�  @��@��P@�C�@��R@�n�@�J@�J@��@��#@��^@��h@��7@��@��@��@��h@���@���@���@��T@��@��@�ff@�M�@�$�@���@��-@�%@��@��u@�j@� �@�  @��m@���@��F@���@�|�@�33@��@�E�@�@�@�`B@�V@��u@�A�@��@���@���@�l�@��\@��^@��/@���@�33@��+@�$�@��@��^@��7@�%@���@�r�@� �@��m@�dZ@���@�M�@�J@��-@���@��h@��@�p�@�O�@���@���@���@��D@��@�z�@�z�@�z�@�j@�j@�Z@�Q�@�9X@�(�@�1@��P@���@�V@�=q@�{@��@��^@�x�@�p�@�G�@��@�bN@� �@�  @���@��m@��
@�t�@���@�-@���@��@��T@��^@��h@�G�@�V@���@���@��@��j@�A�@��m@���@�33@��y@��y@�ȴ@�~�@�-@��@��@��@��@��@���@��@�X@�/@��`@���@��D@��D@�z�@�r�@�r�@�r�@�r�@�1'@���@�t�@�S�@�o@��@��@�ȴ@���@�~�@�5?@��@o�r@]�n1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�+A�-A�1'A�7LA�?}A�E�A�G�A�C�A�C�A�?}A�?}A�?}A�A�A�G�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�K�A�A�A�?}A�33A���Aɡ�A���A�JA��TA���A�jA��#AƑhA�-Aơ�A�\)A�1A�oAƏ\A�+A��A�33A�ZA�l�A�+AĲ-A×�A�`BA���A�%A�A�A��A�9XA�A���A�"�A�5?A�jA�VA�K�A��A�A��9A��A��A��mA�oA���A���A��A��A� �A� �As�hAo�AiO�Ad�!Aa33A_&�A]XAYG�AV=qAT��ANz�AH�/AF��AF-AE33AC�AA7LA@��A@=qA>$�A<��A<$�A;/A9ƨA6ĜA5&�A4E�A2�HA2ĜA2��A1?}A/l�A.��A.v�A.n�A.~�A.JA+�TA+33A)��A'�-A&�\A&VA%�#A!t�A�A��A�RA��AVA�mA��A��A�wAO�A��AS�A�
A�uA�AƨA�AhsAȴAhsAĜA��A�\A�
AG�A%A	A��A�;AO�AS�A�jAM�A{A��A�9A��AS�Av�Ax�A ��A   @���@�v�@��T@�ƨ@���@�
=@��@���@���@�&�@�A�@��@���@�x�@�%@�dZ@�n�@�E�@�@���@�|�@ޗ�@���@�33@���@��@�K�@���@�1'@ӝ�@Ұ!@�v�@љ�@�7L@��`@�b@�V@͑h@�V@�%@�bN@˝�@�o@���@��@ɑh@�V@�Ĝ@�z�@�(�@��@Ǿw@�t�@�\)@�33@�"�@�@�ȴ@ư!@Ƈ+@�ff@Ų-@�V@� �@å�@�t�@�;d@���@¸R@+@�{@�@��-@���@�?}@�Ĝ@��@���@�j@�1@�\)@�@�^5@���@�@��u@�l�@�C�@�
=@�=q@���@��@���@�l�@�+@�~�@�7L@���@�9X@��F@��m@��F@�|�@��@�n�@�hs@�+@�ff@�v�@���@�/@�r�@�  @�\)@�o@�V@���@��!@�5?@�J@���@���@��-@�@�@���@���@���@�`B@�bN@�1'@�1'@�(�@��
@���@�|�@�t�@�t�@�l�@��@��!@�E�@�@��#@��^@��@�&�@��D@�I�@� �@�  @��@��P@�C�@��R@�n�@�J@�J@��@��#@��^@��h@��7@��@��@��@��h@���@���@���@��T@��@��@�ff@�M�@�$�@���@��-@�%@��@��u@�j@� �@�  @��m@���@��F@���@�|�@�33@��@�E�@�@�@�`B@�V@��u@�A�@��@���@���@�l�@��\@��^@��/@���@�33@��+@�$�@��@��^@��7@�%@���@�r�@� �@��m@�dZ@���@�M�@�J@��-@���@��h@��@�p�@�O�@���@���@���@��D@��@�z�@�z�@�z�@�j@�j@�Z@�Q�@�9X@�(�@�1@��P@���@�V@�=q@�{@��@��^@�x�@�p�@�G�@��@�bN@� �@�  @���@��m@��
@�t�@���@�-@���@��@��T@��^@��h@�G�@�V@���@���@��@��j@�A�@��m@���@�33@��y@��y@�ȴ@�~�@�-@��@��@��@��@��@���@��@�X@�/@��`@���@��D@��D@�z�@�r�@�r�@�r�@�r�@�1'@���@�t�@�S�@�o@��@��@�ȴ@���@�~�@�5?@��@o�r@]�n1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
r�B
s�B
s�B
�B
�DB
�VB
��B
ɺB
�fB
��B
��B
��B+B
=B
��B
��BB)�B"�B�B�BH�B�=B��B�jB��B��BBDB1B��B�B��B�sB�B��B�RB��B��B��B��B��B��B��B��B��BÖB�jB�!B�qB�qB�^B�-B��B�B,E�B	��B	�B	�B	bNB	`BB	[#B	S�B	>wB	-B	�B��B�sB�BB�B�HB�B��B��B��BƨBBB��B��B�wB�XB�9B�-B�?B�RB�3B�B��B�B�-B�9BÖB��B��B�qB�B��B��B��B� Bo�Bl�Bm�Bp�Br�Bs�Bp�Bl�BhsBffBffBe`Be`BffBgmBhsBgmBgmBffBe`BbNBbNBe`Bk�Bn�Bq�Bl�Bm�Br�Bw�B�%B�VB�bB�uB�oB�uB�oB�oB�oB�oB�uB�{B��B�{B�uB�{B�{B�oB�\B�\B�bB�bB�bB�hB�hB�hB�hB�oB�{B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�RB�^B�jB�dB�qB�}B��B��BÖBĜBǮBɺB��B��B��B��B��B��B�B�B�
B�B�B�#B�5B�ZB�sB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	%B	+B		7B	1B	
=B	PB	VB	bB	�B	�B	�B	"�B	(�B	33B	7LB	9XB	<jB	A�B	E�B	F�B	J�B	N�B	Q�B	T�B	VB	XB	[#B	^5B	`BB	e`B	gmB	iyB	gmB	ffB	iyB	p�B	u�B	|�B	�B	�DB	�DB	�DB	�DB	�JB	�JB	�PB	�PB	�PB	�PB	�PB	�bB	�VB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�XB	�XB	�^B	�^B	�dB	�jB	�qB	�qB	�wB	��B	ÖB	ŢB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�;B	�BB	�HB	�TB	�TB	�ZB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
\B
bB
bB
bB
hB
hB
hB
hB
�B
�B
 �B
($2222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
r�B
s�B
s�B
�B
�DB
�VB
��B
ɺB
�fB
��B
��B
��B+B
=B
��B
��BB)�B"�B�B�BH�B�=B��B�jB��B��BBDB1B��B�B��B�sB�B��B�RB��B��B��B��B��B��B��B��B��BÖB�jB�!B�qB�qB�^B�-B��B�B,E�B	��B	�B	�B	bNB	`BB	[#B	S�B	>wB	-B	�B��B�sB�BB�B�HB�B��B��B��BƨBBB��B��B�wB�XB�9B�-B�?B�RB�3B�B��B�B�-B�9BÖB��B��B�qB�B��B��B��B� Bo�Bl�Bm�Bp�Br�Bs�Bp�Bl�BhsBffBffBe`Be`BffBgmBhsBgmBgmBffBe`BbNBbNBe`Bk�Bn�Bq�Bl�Bm�Br�Bw�B�%B�VB�bB�uB�oB�uB�oB�oB�oB�oB�uB�{B��B�{B�uB�{B�{B�oB�\B�\B�bB�bB�bB�hB�hB�hB�hB�oB�{B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�RB�^B�jB�dB�qB�}B��B��BÖBĜBǮBɺB��B��B��B��B��B��B�B�B�
B�B�B�#B�5B�ZB�sB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	%B	+B		7B	1B	
=B	PB	VB	bB	�B	�B	�B	"�B	(�B	33B	7LB	9XB	<jB	A�B	E�B	F�B	J�B	N�B	Q�B	T�B	VB	XB	[#B	^5B	`BB	e`B	gmB	iyB	gmB	ffB	iyB	p�B	u�B	|�B	�B	�DB	�DB	�DB	�DB	�JB	�JB	�PB	�PB	�PB	�PB	�PB	�bB	�VB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�XB	�XB	�^B	�^B	�dB	�jB	�qB	�qB	�wB	��B	ÖB	ŢB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�;B	�BB	�HB	�TB	�TB	�ZB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
\B
bB
bB
bB
hB
hB
hB
hB
�B
�B
 �B
($2222222222222222222222222222222222222222222222222222222222222222222222222422222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.07 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190459                              AO  ARCAADJP                                                                    20181005190459    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190459  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190459  QCF$                G�O�G�O�G�O�8000            