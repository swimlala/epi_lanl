CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-17T09:15:48Z AOML 3.0 creation; 2016-08-07T21:36:47Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160617091548  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               |A   AO  5286_8897_124                   2C  D   APEX                            6531                            072314                          846 @״��&�Q1   @״��b�A@4��vȴ9�c9�$�/1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    |A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dys3D�	�D�FfD���D�� D��D�I�D�� D�ٚD�  D�33D��3D��fD� D�,�Dڙ�D��3D�	�D�L�D�|�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@��HAp�A!p�AAp�Aap�A��RA��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C0�C
C
C

C
C
C
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
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CJ
CL
CN
CP
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
C~
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN]DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DW�]DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtx�Dyx�D�{D�IGD��{D���D��D�L{D���D��{D��D�6D��D��GD��D�/�Dڜ{D��D�{D�O�D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A���A���A�ĜA�AΟ�A΋DA�p�A�S�A�;dA�(�A��A��A�oA�
=A�A���A��mA��A˺^Aʧ�A���AȓuA���A��#A�`BA��A�-A��AĴ9A�ZA�%Aú^A�1'A�A�x�A�(�A�&�A�^5A�+A���A�bNA�
=A���A�jA��PA�$�A��A��A�+A�;dA�p�A�~�A�XA�dZA�=qA�ĜA�VA��A��^A��;A��A�A�JA�M�A�{A��A���A���A��+A�n�A�n�A��`A�(�A���A�9XA�&�A���A� �A�33A��^A� �A�ffA��\A�=qA��#A��wA���A��A�A�A���A���A���A���A�x�A�ȴA��9A�&�A��wA��HA��-A���A�33A�K�A}��A}%A|n�A|JAyoAs�Ao/Al�jAi?}AeAb��A_�FA^��A]�A\$�AY�FAW��AU�AQ|�AO�AMG�ALv�AJ��AE��AB��AAp�AA�A@�jA?��A<�`A;dZA:z�A:1A7"�A4�A2�RA2��A2bNA1�;A1��A1;dA0bNA.��A.9XA,�\A+hsA*z�A(�HA'�PA'G�A'%A&�DA%&�A"�HA!33A��A1'AAoA=qA�A��A=qAbA�A�A��A�^AoA�A-A�A
��A
-A	�PA	?}A	A��A$�AXA�A?}A	VA5?AdZA\)AA��A`BA�AO�A��A-Ap�AĜA �A��A ��@��@�M�@�Z@�+@�J@��@�7L@�^5@�
=@��h@��-@��h@���@��@��h@�9X@�@���@�P@�o@�n�@�@���@�h@�!@��@��@�7@���@�&�@��@�
=@�7@�z�@�1'@�
=@���@�/@�&�@�&�@���@�A�@�\)@��@�9X@��`@�+@��`@�-@�&�@�bN@��@϶F@�|�@�K�@��@ͩ�@���@̣�@��@�J@��@�?}@�1'@Ǿw@�"�@�"�@�^5@�V@�@őh@�A�@�@���@�v�@���@��@���@���@�@��R@���@�n�@���@��7@�&�@�X@��-@�hs@�%@��@���@�l�@�S�@�
=@���@���@��m@�S�@���@�@�=q@�^5@�ff@��@�7L@�b@���@���@��P@�K�@��@�ȴ@�E�@�
=@�l�@�t�@��@���@�ƨ@��w@��@�^5@���@���@�l�@�o@�
=@�+@�C�@�dZ@�
=@���@�^5@���@�x�@�  @��!@�@�t�@�t�@�33@��@�@��@�ȴ@�v�@�$�@�@���@��-@��T@���@���@�X@�7L@�%@��@�Q�@�b@���@�S�@��@���@��T@��7@�?}@�V@��`@��@�z�@�  @��P@�\)@�S�@�C�@�33@�o@�
=@���@�~�@��T@�G�@�%@��u@�bN@�1'@���@�dZ@�33@�ȴ@�E�@�{@��T@��^@���@��7@�`B@�V@��u@�Q�@��m@��F@�;d@�@�n�@�$�@��@��#@�x�@�V@���@��@���@��D@�z�@�bN@�I�@�ƨ@��@�t�@�dZ@�@�v�@�M�@�5?@���@���@�p�@���@��j@�r�@�  @�ƨ@�|�@�K�@�o@�~�@��T@�@��@�O�@���@��D@��@�z�@�Z@�b@��w@��P@�t�@�33@�
=@��@��@��\@�M�@�{@��-@���@��h@�&�@��`@��j@��9@�bN@� �@��F@�\)@�+@�@�n�@��@��#@�hs@�G�@�%@��j@�j@��@���@���@���@���@�ƨ@���@�V@{C�@t�@j��@b�\@Y��@Q&�@J��@C�F@>ȴ@8�u@0��@,j@';d@"M�@�P@9X@K�@M�@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��A��A���A���A�ĜA�AΟ�A΋DA�p�A�S�A�;dA�(�A��A��A�oA�
=A�A���A��mA��A˺^Aʧ�A���AȓuA���A��#A�`BA��A�-A��AĴ9A�ZA�%Aú^A�1'A�A�x�A�(�A�&�A�^5A�+A���A�bNA�
=A���A�jA��PA�$�A��A��A�+A�;dA�p�A�~�A�XA�dZA�=qA�ĜA�VA��A��^A��;A��A�A�JA�M�A�{A��A���A���A��+A�n�A�n�A��`A�(�A���A�9XA�&�A���A� �A�33A��^A� �A�ffA��\A�=qA��#A��wA���A��A�A�A���A���A���A���A�x�A�ȴA��9A�&�A��wA��HA��-A���A�33A�K�A}��A}%A|n�A|JAyoAs�Ao/Al�jAi?}AeAb��A_�FA^��A]�A\$�AY�FAW��AU�AQ|�AO�AMG�ALv�AJ��AE��AB��AAp�AA�A@�jA?��A<�`A;dZA:z�A:1A7"�A4�A2�RA2��A2bNA1�;A1��A1;dA0bNA.��A.9XA,�\A+hsA*z�A(�HA'�PA'G�A'%A&�DA%&�A"�HA!33A��A1'AAoA=qA�A��A=qAbA�A�A��A�^AoA�A-A�A
��A
-A	�PA	?}A	A��A$�AXA�A?}A	VA5?AdZA\)AA��A`BA�AO�A��A-Ap�AĜA �A��A ��@��@�M�@�Z@�+@�J@��@�7L@�^5@�
=@��h@��-@��h@���@��@��h@�9X@�@���@�P@�o@�n�@�@���@�h@�!@��@��@�7@���@�&�@��@�
=@�7@�z�@�1'@�
=@���@�/@�&�@�&�@���@�A�@�\)@��@�9X@��`@�+@��`@�-@�&�@�bN@��@϶F@�|�@�K�@��@ͩ�@���@̣�@��@�J@��@�?}@�1'@Ǿw@�"�@�"�@�^5@�V@�@őh@�A�@�@���@�v�@���@��@���@���@�@��R@���@�n�@���@��7@�&�@�X@��-@�hs@�%@��@���@�l�@�S�@�
=@���@���@��m@�S�@���@�@�=q@�^5@�ff@��@�7L@�b@���@���@��P@�K�@��@�ȴ@�E�@�
=@�l�@�t�@��@���@�ƨ@��w@��@�^5@���@���@�l�@�o@�
=@�+@�C�@�dZ@�
=@���@�^5@���@�x�@�  @��!@�@�t�@�t�@�33@��@�@��@�ȴ@�v�@�$�@�@���@��-@��T@���@���@�X@�7L@�%@��@�Q�@�b@���@�S�@��@���@��T@��7@�?}@�V@��`@��@�z�@�  @��P@�\)@�S�@�C�@�33@�o@�
=@���@�~�@��T@�G�@�%@��u@�bN@�1'@���@�dZ@�33@�ȴ@�E�@�{@��T@��^@���@��7@�`B@�V@��u@�Q�@��m@��F@�;d@�@�n�@�$�@��@��#@�x�@�V@���@��@���@��D@�z�@�bN@�I�@�ƨ@��@�t�@�dZ@�@�v�@�M�@�5?@���@���@�p�@���@��j@�r�@�  @�ƨ@�|�@�K�@�o@�~�@��T@�@��@�O�@���@��D@��@�z�@�Z@�b@��w@��P@�t�@�33@�
=@��@��@��\@�M�@�{@��-@���@��h@�&�@��`@��j@��9@�bN@� �@��F@�\)@�+@�@�n�@��@��#@�hs@�G�@�%@��j@�j@��@���@���@���@���G�O�@���@�V@{C�@t�@j��@b�\@Y��@Q&�@J��@C�F@>ȴ@8�u@0��@,j@';d@"M�@�P@9X@K�@M�@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�5B
+B
VB
VB
�B
�B
�B
1'B
0!B
%�B
+B
)�B
+B
-B
33B
?}B
W
B
cTB
v�B
�DB
�B
�dB
�XB
�wB
ŢB
��B
�HB
=BVBH�Bw�B��BŢB�B�B	7B�B%�B0!B0!B �BoB�B)�B-B�BB��B��Br�B[#B[#BC�BD�BbNB}�B�JB�Bm�BO�B�B
��B
�B
�B
��B
�BB
�LB
�{B
�VB
��B
�\B
u�B
`BB
M�B
<jB
!�B
bB	��B	�B	��B	��B	�=B	�B	|�B	bNB	ZB	P�B	W
B	YB	S�B	D�B	!�B	B�B��B�RB��B��B�{B�oB�VB�PB�7B�Bx�Br�Bm�BiyBaHBS�BP�BS�BiyB~�B�B��B��B��B��B�+B�B�DB��B��B�^B�jB�}BǮBǮB��B��B��B�B�#B�)B�#B�#B�B��B��BɺB��B��B��B�B�B�B��B��BÖB�RB��B�7B�+B�Bs�Bn�Bk�BiyBhsBe`BcTBcTBbNBbNBdZBiyBp�B�PB�VB�=B�PB�bB�JB�JB��B��B��B��B��B��B��B��B�{B�BbNBN�BG�BG�BF�BJ�BW
B�B��B��B��B��B�-BĜBɺBƨB��B��BǮBĜB��B�}B�dB�XBB��B��B��B�/B�NB�ZB�fB�mB�sB�sB�B�B�B�B�B�B�B�B�mB�5B�B��BɺB��B��B��B��B��B��B��B��B��B��B��B�B�)B�;B�BB�NB�`B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B	B	
=B	PB	VB	bB	�B	�B	 �B	!�B	#�B	$�B	$�B	#�B	%�B	'�B	,B	0!B	1'B	0!B	/B	0!B	2-B	2-B	33B	33B	49B	5?B	6FB	?}B	F�B	F�B	G�B	H�B	H�B	I�B	J�B	I�B	H�B	H�B	I�B	J�B	M�B	N�B	O�B	O�B	S�B	VB	XB	\)B	^5B	]/B	_;B	bNB	e`B	k�B	o�B	p�B	p�B	q�B	s�B	w�B	w�B	x�B	y�B	~�B	�B	�7B	�DB	�JB	�PB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�?B	�LB	�RB	�XB	�XB	�^B	�dB	�jB	�jB	�jB	�wB	�wB	�}B	��B	B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
1B
hB
�B
 �B
'�B
/B
6FB
;dB
F�B
L�B
P�B
W
B
\)B
^5B
aHB
e`B
ffB
k�B
o�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�7B
.B
WB
WB
�B
�B
�B
1&B
0B
%�B
+B
)�B
+B
-B
34B
?xB
W
B
cRB
v�B
�?B
��B
�`B
�VB
�sB
ŞB
��B
�FB
5BRBH�Bw�B��BŚB�B�B	0B�B%�B0B0B �BiB�B)�B-B�BB��B��Br�B[B[BC�BD�BbGB}�B�BB�
Bm�BO�B�B
��B
�B
�B
��B
�;B
�FB
�wB
�RB
��B
�XB
u�B
`?B
M�B
<hB
!�B
aB	��B	�B	��B	��B	�BB	�B	|�B	bQB	Z$B	P�B	WB	YB	S�B	D�B	!�B	B�B��B�^B��B��B��B�~B�cB�]B�EB�!Bx�Br�Bm�Bi�BaVBT	BP�BT	Bi�BB�*B��B��B��B��B�9B� B�QB��B� B�gB�vB��BǴBǷB��B��B��B�B�+B�0B�*B�*B�B��B��B��B��B��B��B�&B�B�
B��B��BÝB�[B��B�BB�7B�Bs�Bn�Bk�Bi�Bh�BenBc`BcaBb]BbZBddBi�Bp�B�ZB�^B�HB�\B�lB�RB�SB��B��B��B��B��B��B��B��B��B�BbXBN�BG�BG�BF�BJ�BWB�B��B��B��B��B�5BĤB��BƮB��B��BǲBĤB��B��B�lB�^BB��B��B�B�6B�TB�_B�lB�pB�wB�xB�B�B�B�B�B�B�B�B�rB�;B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�0B�@B�FB�TB�eB�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B	B	
?B	SB	XB	eB	�B	�B	 �B	!�B	#�B	$�B	$�B	#�B	%�B	'�B	,B	0"B	1)B	0#B	/B	0#B	2+B	2-B	34B	33B	4;B	5AB	6HB	?�B	F�B	F�B	G�B	H�B	H�B	I�B	J�B	I�B	H�B	H�B	I�B	J�B	M�B	N�B	O�B	O�B	S�B	VB	XB	\(B	^3B	]0B	_:B	bOB	e`B	k�B	o�B	p�B	p�B	q�B	s�B	w�B	w�B	x�B	y�B	~�B	�B	�7B	�CB	�GB	�MB	�ZB	�aB	�nB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�B	�&B	�)B	�;B	�IB	�OB	�VB	�TB	�[B	�cB	�hB	�fB	�gB	�rB	�sB	�yB	��B	B	B	ÒB	ęB	ƥB	ǫB	ȯB	ȰB	ɶB	ɷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�&B	�2B	�3B	�7B	�@B	�DB	�DB	�IB	�NB	�TB	�aB	�bB	�gB	�nB	�uB	�xB	�xB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
,B
cB
�B
 �B
'�B
/B
6>B
;aB
F�B
L�B
P�B
WB
\!B
^.B
a>B
eXB
f_B
k}B
o�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.09 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436472016080714364720160807143647  AO  ARCAADJP                                                                    20160617091548    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160617091548  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160617091548  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143647  IP                  G�O�G�O�G�O�                