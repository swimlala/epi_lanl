CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:41Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140841  20181024140841  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$�)�1   @��&`� @5S�����d���S�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   B   B   @&ff@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C�fC�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C;�fC>  C@  CA�fCD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� DfD� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3y�D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;�fD<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dj��Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv� Dw  Dw� Dw�fDy�qD�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @,(�@��H@��HAp�A!p�AC
>Aap�A��RA��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�aGB�aGB�.B���B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C�pC
C
C

C�pC�pC
C
C
C
C
C
C
C
C 
C"
C$
C&
C(
C*
C,
C.
C0
C20�C4
C6
C8
C:
C;�pC>
C@
CA�pCD
CF
CH
CJ
CL
CN0�CP
CR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C}�pC��C��C��C��C��C��C��C��C��C���C��C�RC��C��C��C��C�RC�RC��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C���C��C�RC��C��C��C��C��C���C���C��C�RC��C��C�RC��C��C���C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C�RC��C��C��D �D ��D�D��D��D�D��D�D��D�D��D�D��D�D��D�]D]D�D��D�D��D)D��D�D��D�D]D�]D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!)D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&�)D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D+�]D,��D-)D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2�)D3�D3]D4�D4��D5�D5��D6�D6��D7�D7�)D8�D8��D9�D9��D:�D:��D;�D;�)D<�D<]D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN)DN�)DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[�)D\)D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di�)Dj�Dj��Dj�]Dk]Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq)Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�]Du��Dv�Dv��Dw�Dw��Dw�)Dy�4D�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�A�A�?}A�A�A�A�A�C�A�A�A�C�A�C�A�A�A�C�A�C�A�C�A�E�A�C�A�A�A�33A� �A��
A���AΩ�A�ĜA͙�A͇+A�|�A�t�A�jA�$�A���A�t�A�$�A˕�A�oA�ƨA�
=A�ZA�O�A��`A�`BA��^A�dZA�?}A�VA���A�C�A�t�A�%A���A�~�A���A�XA���A�S�A�-A�1A��HA�A��A�K�A���A��A��A���A���A�x�A�"�A�=qA�ffA���A�/A���A��`A�-A�JA�\)A���A���A�+A�z�A�-A���A�XA�~�A���A��HA�hsA�A�x�A���A��9A�A��RA��A�;dA��`A�  A��uA�K�A�
=A~��Az(�At��As`BAp�Ao�#Am�Ak�PAh��Af(�Ae�-Ae�Ab�yA`��A^AZr�AX�uAV��AR�`AO��AN�AMS�AL{AK�
AK�PAJQ�AH=qAGVAF��AF-AE�mAD�AD9XAB��AAS�A@�uA>v�A<I�A;��A;x�A:��A:�DA:5?A9�;A97LA8ffA7
=A4��A3"�A2��A2=qA1/A0^5A.(�A*z�A(ȴA&�yA%x�A$��A$VA#��A"�`A"A�A!��A!�hA!;dA!A ��A ffA��A�A�`AĜA�!A��A��A�DA1'A;dA��Ar�A1A��A�A��A�-AĜA�AE�AĜAv�A�-A
bNA	A��A��AbNAQ�AI�A �A&�A�AdZA�A�hA �yA �A M�@�t�@�{@�dZ@��@���@�t�@�{@��@��T@��@�@�5?@�-@�-@�J@�O�@�1'@�"�@�ff@陚@�O�@��@�1'@�P@��@��@�-@噚@�/@�w@���@���@�b@�;d@�E�@���@۾w@�;d@���@���@ڏ\@ف@؛�@׾w@��@ՙ�@�`B@��@ӝ�@�O�@��m@��@�@��+@�J@���@��^@��@�1'@�j@�b@�G�@��j@�(�@��+@�%@�|�@�o@�v�@�$�@���@���@�I�@�K�@���@�{@�@�`B@���@���@�@�x�@��T@��@���@�
=@�;d@�K�@���@��`@�$�@��!@���@�1'@�K�@�dZ@��D@�Q�@��@�Q�@�@� �@��R@�Ĝ@��@�v�@��\@��+@���@��y@�"�@�n�@���@���@�o@�S�@�t�@��P@��@���@� �@�(�@��m@���@��P@��P@�K�@��@���@��@�Z@�bN@��u@��@��@���@��@�r�@�I�@�  @��;@���@�dZ@�-@��#@���@�V@�I�@�l�@�+@�ff@���@���@�x�@�O�@�/@�V@��/@��/@��@��@�Q�@�(�@�1@�1@�  @� �@�Z@��D@�Z@�dZ@�ff@�5?@�{@�@��T@��@�7L@��@���@��u@�I�@��@���@�33@��y@��@��R@�V@��@��@���@��@�x�@�`B@�G�@���@���@�j@��;@�S�@�o@��\@��@��T@��#@���@��-@��@�&�@���@�bN@�(�@�1@��;@��w@��@���@���@��P@�|�@�dZ@�S�@�
=@���@�v�@���@�x�@�x�@���@�@��T@��@��-@���@�x�@�O�@�?}@�/@��@���@�I�@���@�ƨ@��@���@��P@��@�S�@�
=@��!@���@�v�@�V@�5?@�-@��T@�@�x�@�G�@���@�bN@�I�@� �@���@��P@�@{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�33A�A�A�?}A�A�A�A�A�C�A�A�A�C�A�C�A�A�A�C�A�C�A�C�A�E�A�C�A�A�A�33A� �A��
A���AΩ�A�ĜA͙�A͇+A�|�A�t�A�jA�$�A���A�t�A�$�A˕�A�oA�ƨA�
=A�ZA�O�A��`A�`BA��^A�dZA�?}A�VA���A�C�A�t�A�%A���A�~�A���A�XA���A�S�A�-A�1A��HA�A��A�K�A���A��A��A���A���A�x�A�"�A�=qA�ffA���A�/A���A��`A�-A�JA�\)A���A���A�+A�z�A�-A���A�XA�~�A���A��HA�hsA�A�x�A���A��9A�A��RA��A�;dA��`A�  A��uA�K�A�
=A~��Az(�At��As`BAp�Ao�#Am�Ak�PAh��Af(�Ae�-Ae�Ab�yA`��A^AZr�AX�uAV��AR�`AO��AN�AMS�AL{AK�
AK�PAJQ�AH=qAGVAF��AF-AE�mAD�AD9XAB��AAS�A@�uA>v�A<I�A;��A;x�A:��A:�DA:5?A9�;A97LA8ffA7
=A4��A3"�A2��A2=qA1/A0^5A.(�A*z�A(ȴA&�yA%x�A$��A$VA#��A"�`A"A�A!��A!�hA!;dA!A ��A ffA��A�A�`AĜA�!A��A��A�DA1'A;dA��Ar�A1A��A�A��A�-AĜA�AE�AĜAv�A�-A
bNA	A��A��AbNAQ�AI�A �A&�A�AdZA�A�hA �yA �A M�@�t�@�{@�dZ@��@���@�t�@�{@��@��T@��@�@�5?@�-@�-@�J@�O�@�1'@�"�@�ff@陚@�O�@��@�1'@�P@��@��@�-@噚@�/@�w@���@���@�b@�;d@�E�@���@۾w@�;d@���@���@ڏ\@ف@؛�@׾w@��@ՙ�@�`B@��@ӝ�@�O�@��m@��@�@��+@�J@���@��^@��@�1'@�j@�b@�G�@��j@�(�@��+@�%@�|�@�o@�v�@�$�@���@���@�I�@�K�@���@�{@�@�`B@���@���@�@�x�@��T@��@���@�
=@�;d@�K�@���@��`@�$�@��!@���@�1'@�K�@�dZ@��D@�Q�@��@�Q�@�@� �@��R@�Ĝ@��@�v�@��\@��+@���@��y@�"�@�n�@���@���@�o@�S�@�t�@��P@��@���@� �@�(�@��m@���@��P@��P@�K�@��@���@��@�Z@�bN@��u@��@��@���@��@�r�@�I�@�  @��;@���@�dZ@�-@��#@���@�V@�I�@�l�@�+@�ff@���@���@�x�@�O�@�/@�V@��/@��/@��@��@�Q�@�(�@�1@�1@�  @� �@�Z@��D@�Z@�dZ@�ff@�5?@�{@�@��T@��@�7L@��@���@��u@�I�@��@���@�33@��y@��@��R@�V@��@��@���@��@�x�@�`B@�G�@���@���@�j@��;@�S�@�o@��\@��@��T@��#@���@��-@��@�&�@���@�bN@�(�@�1@��;@��w@��@���@���@��P@�|�@�dZ@�S�@�
=@���@�v�@���@�x�@�x�@���@�@��T@��@��-@���@�x�@�O�@�?}@�/@��@���@�I�@���@�ƨ@��@���@��P@��@�S�@�
=@��!@���@�v�@�V@�5?@�-@��T@�@�x�@�G�@���@�bN@�I�@� �@���@��P@�@{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�B
�B
�`B�BXB��B�FB�}B��BBŢB�)B�BJB#�BA�B]/BM�B=qB(�BH�BP�BXBVBW
B^5BhsBm�Bm�Bu�Bo�Bo�Bu�Bu�Bp�Bn�Bn�Bm�Bk�BjBcTBXBH�B@�B1'B(�B%�B"�B�B�B\BB��B��B  B��B�mB��BɺB�XB��B��B�{B��B�\B�B{�Bz�Bp�B_;B=qB%B
�B
�/B
��B
�XB
��B
�B
q�B
_;B
?}B
,B
�B
hB	��B	��B	��B	�3B	�B	��B	�hB	� B	m�B	hsB	dZB	Q�B	C�B	6FB	,B	%�B	�B		7B	%B	VB	PB	1B	+B	B	B��B��B��B�B�B�B�B�fB�BB�)B��B��B��B��BȴBƨBŢBÖB��B�jB�?B�-B�!B�B�B�B��B��B��B��B��B��B��B�{B�hB�VB�JB�JB�PB�JB�JB�DB�=B�1B�%B�B�B�B�B�B�B�B�B� B~�B~�B~�By�Bs�Bo�BjBdZB]/B_;B_;B_;B^5B^5B\)B]/B]/B]/B]/B]/B^5B^5B]/B[#BZBZBZBYBYBYB[#B\)B\)B\)B\)B]/B`BBe`BhsBiyBiyBhsBhsBhsBhsBhsBgmBffBffBgmBgmBgmBgmBhsBhsBhsBhsBiyBiyBm�Bn�Bn�Bm�Bn�Bp�Bq�Bq�Bq�Bq�Bv�B{�B�B�7B�1B�+B�B�B{�B|�BG�B�=B�7B�7B�7B�7B�7B�DB�hB��B�oB�bB�\B�=B�%B�B�B�B�B�+B�7B�DB�uB��B��B��B��B��B�!B�RB�qBǮB��B�/B�TB�mB�B�B��B	VB	uB	�B	!�B	"�B	$�B	33B	8RB	;dB	9XB	33B	'�B	'�B	8RB	C�B	H�B	K�B	N�B	P�B	R�B	T�B	VB	ZB	]/B	gmB	jB	l�B	m�B	o�B	t�B	w�B	{�B	~�B	� B	�B	�B	�B	�B	�B	� B	~�B	� B	� B	�B	�B	�B	� B	�B	� B	� B	� B	� B	�B	�%B	�1B	�7B	�DB	�VB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�?B	�XB	�wB	�}B	�}B	�}B	��B	ŢB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
GB
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
�B
�B
�`B�BXB��B�FB�}B��BBŢB�)B�BJB#�BA�B]/BM�B=qB(�BH�BP�BXBVBW
B^5BhsBm�Bm�Bu�Bo�Bo�Bu�Bu�Bp�Bn�Bn�Bm�Bk�BjBcTBXBH�B@�B1'B(�B%�B"�B�B�B\BB��B��B  B��B�mB��BɺB�XB��B��B�{B��B�\B�B{�Bz�Bp�B_;B=qB%B
�B
�/B
��B
�XB
��B
�B
q�B
_;B
?}B
,B
�B
hB	��B	��B	��B	�3B	�B	��B	�hB	� B	m�B	hsB	dZB	Q�B	C�B	6FB	,B	%�B	�B		7B	%B	VB	PB	1B	+B	B	B��B��B��B�B�B�B�B�fB�BB�)B��B��B��B��BȴBƨBŢBÖB��B�jB�?B�-B�!B�B�B�B��B��B��B��B��B��B��B�{B�hB�VB�JB�JB�PB�JB�JB�DB�=B�1B�%B�B�B�B�B�B�B�B�B� B~�B~�B~�By�Bs�Bo�BjBdZB]/B_;B_;B_;B^5B^5B\)B]/B]/B]/B]/B]/B^5B^5B]/B[#BZBZBZBYBYBYB[#B\)B\)B\)B\)B]/B`BBe`BhsBiyBiyBhsBhsBhsBhsBhsBgmBffBffBgmBgmBgmBgmBhsBhsBhsBhsBiyBiyBm�Bn�Bn�Bm�Bn�Bp�Bq�Bq�Bq�Bq�Bv�B{�B�B�7B�1B�+B�B�B{�B|�BG�B�=B�7B�7B�7B�7B�7B�DB�hB��B�oB�bB�\B�=B�%B�B�B�B�B�+B�7B�DB�uB��B��B��B��B��B�!B�RB�qBǮB��B�/B�TB�mB�B�B��B	VB	uB	�B	!�B	"�B	$�B	33B	8RB	;dB	9XB	33B	'�B	'�B	8RB	C�B	H�B	K�B	N�B	P�B	R�B	T�B	VB	ZB	]/B	gmB	jB	l�B	m�B	o�B	t�B	w�B	{�B	~�B	� B	�B	�B	�B	�B	�B	� B	~�B	� B	� B	�B	�B	�B	� B	�B	� B	� B	� B	� B	�B	�%B	�1B	�7B	�DB	�VB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�?B	�XB	�wB	�}B	�}B	�}B	��B	ŢB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
GB
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140841                              AO  ARCAADJP                                                                    20181024140841    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140841  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140841  QCF$                G�O�G�O�G�O�4000            