CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-13T09:15:57Z AOML 3.0 creation; 2016-08-07T21:36:39Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151013091557  20160807143640  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               MA   AO  5286_8897_077                   2C  D   APEX                            6531                            072314                          846 @�v��<�1   @�v�-��	@2N��+�c<9XbN1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    MA   B   B   @���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDys3D��D�L�D��3D���D� D�<�D�p D�� D�	�D�C3D��fD�� D�3D�@ Dړ3D��D�fD�6fD�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��HAp�A�
AAp�Aap�A��RA��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C
C
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
C�pC
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
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK)DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�)Dyx�D��D�O�D��D���D��D�?�D�r�D���D�{D�FD��GD���D�D�B�DږD࿮D�	GD�9GD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��TA��yA��A��/Aݴ9AݮA݅A�\)A�;dA�(�A��A�A��HAܧ�A�p�A�+A�ĜAە�A�^5A�A���A���A��
A���Aק�A��AҾwA���A�oA���A��A�
=A�hsA�l�A�v�A�%A�l�A��A�p�Aƙ�AżjA�VAģ�A�JAÑhA��`A���A�1A�dZA�{A��
A��yA���A�;dA���A�ƨA�|�A�XA� �A�9XA��A�G�A�t�A��^A��PA��RA�`BA��A���A���A�p�A�`BA���A�v�A�A�&�A�t�A���A�ZA�A�/A���A��A�bNA�G�A��A�$�A�VA��^A�I�A���A���A�1'A�33A�-A��+A��A�p�A���A���A�r�A��A��uA��HA���A�A�A�Q�A|v�Az$�Aw"�AuK�An��Ak�Ah�Af�yAcp�A`�HA_�wA]��A\�\AY%AV��ATVAS�FAQ��AP  AO7LAM�#AL��AKoAIoAG��AG�hADn�ABZAA��A?��A<�A:VA8 �A7&�A5`BA3�TA2~�A0ffA/?}A.��A.��A-��A,ZA*�`A*bA)�A(I�A'G�A&�\A%�A$�A$��A$�uA$M�A!��A JAhsA"�A�RA�AM�A �A;dA��AVAA�wA|�A��AO�A�#A`BA�;Az�A%A��A��A�A��A"�A  AVA
��A
^5A
9XA
A	O�A��A�A��A{AA��A%AA�A��AoA ��A M�@���@�+@�
=@��^@��u@�ƨ@��H@��!@�v�@���@�V@��;@���@��R@��+@���@���@��@��@�x�@��@�w@��y@@��@�hs@�j@��@�^5@�x�@��@�"�@�@�F@�M�@�M�@�ff@�5?@��/@ߍP@ާ�@�@ۍP@�I�@׾w@�S�@�@�?}@���@���@ԛ�@�9X@�(�@�l�@���@щ7@Ѳ-@�J@��#@�O�@�Q�@�;d@�C�@�+@��@��@�o@�v�@�x�@�r�@�ƨ@�{@�x�@͡�@ΰ!@�o@υ@�@�O�@Ǿw@���@¸R@�{@���@��T@�b@�E�@�x�@�?}@���@�hs@��T@�/@��@��D@�b@��@��H@��@��w@�hs@�%@���@��@��w@��@��h@�`B@���@��@���@�j@���@��m@��P@�S�@���@��@���@�Ĝ@��D@�Ĝ@�O�@�p�@�hs@���@�j@�b@��F@�~�@��@�o@�ȴ@���@��P@�@��@��@�M�@��@��@�r�@��@�t�@�+@�o@�
=@�
=@���@��\@��#@��@�hs@�/@�bN@��;@���@��@�dZ@�
=@��@��y@��y@�n�@�~�@��R@�^5@��D@���@�
=@�ff@���@�hs@���@��u@�b@���@��@��m@�ƨ@�ƨ@��F@�t�@�C�@��@���@�M�@�-@��@��-@���@��@�O�@�%@�7L@�&�@��@��@���@���@���@��@�z�@��u@�I�@�9X@� �@��;@��P@�dZ@�C�@���@��+@���@�=q@�V@�-@�@���@�x�@�X@�?}@�V@��j@�z�@�I�@�b@��;@��@�\)@�C�@��@�
=@�@��@��y@�ȴ@��!@�~�@�V@�-@�M�@�l�@�1@��@�I�@�j@�;d@�v�@�ȴ@�o@��@�ȴ@��\@��R@���@�ƨ@�Ĝ@��j@�(�@�|�@�;d@�"�@��H@��!@�M�@�J@��@���@�/@��F@�n�@�E�@��^@��@��^@���@���@��-@�@���@�/@�@�`B@�@w�@h��@bJ@[t�@O�;@J^5@B�!@;dZ@4�@-�h@'
=@!��@�j@X@Z@X@E�@
�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��HA��TA��yA��A��/Aݴ9AݮA݅A�\)A�;dA�(�A��A�A��HAܧ�A�p�A�+A�ĜAە�A�^5A�A���A���A��
A���Aק�A��AҾwA���A�oA���A��A�
=A�hsA�l�A�v�A�%A�l�A��A�p�Aƙ�AżjA�VAģ�A�JAÑhA��`A���A�1A�dZA�{A��
A��yA���A�;dA���A�ƨA�|�A�XA� �A�9XA��A�G�A�t�A��^A��PA��RA�`BA��A���A���A�p�A�`BA���A�v�A�A�&�A�t�A���A�ZA�A�/A���A��A�bNA�G�A��A�$�A�VA��^A�I�A���A���A�1'A�33A�-A��+A��A�p�A���A���A�r�A��A��uA��HA���A�A�A�Q�A|v�Az$�Aw"�AuK�An��Ak�Ah�Af�yAcp�A`�HA_�wA]��A\�\AY%AV��ATVAS�FAQ��AP  AO7LAM�#AL��AKoAIoAG��AG�hADn�ABZAA��A?��A<�A:VA8 �A7&�A5`BA3�TA2~�A0ffA/?}A.��A.��A-��A,ZA*�`A*bA)�A(I�A'G�A&�\A%�A$�A$��A$�uA$M�A!��A JAhsA"�A�RA�AM�A �A;dA��AVAA�wA|�A��AO�A�#A`BA�;Az�A%A��A��A�A��A"�A  AVA
��A
^5A
9XA
A	O�A��A�A��A{AA��A%AA�A��AoA ��A M�@���@�+@�
=@��^@��u@�ƨ@��H@��!@�v�@���@�V@��;@���@��R@��+@���@���@��@��@�x�@��@�w@��y@@��@�hs@�j@��@�^5@�x�@��@�"�@�@�F@�M�@�M�@�ff@�5?@��/@ߍP@ާ�@�@ۍP@�I�@׾w@�S�@�@�?}@���@���@ԛ�@�9X@�(�@�l�@���@щ7@Ѳ-@�J@��#@�O�@�Q�@�;d@�C�@�+@��@��@�o@�v�@�x�@�r�@�ƨ@�{@�x�@͡�@ΰ!@�o@υ@�@�O�@Ǿw@���@¸R@�{@���@��T@�b@�E�@�x�@�?}@���@�hs@��T@�/@��@��D@�b@��@��H@��@��w@�hs@�%@���@��@��w@��@��h@�`B@���@��@���@�j@���@��m@��P@�S�@���@��@���@�Ĝ@��D@�Ĝ@�O�@�p�@�hs@���@�j@�b@��F@�~�@��@�o@�ȴ@���@��P@�@��@��@�M�@��@��@�r�@��@�t�@�+@�o@�
=@�
=@���@��\@��#@��@�hs@�/@�bN@��;@���@��@�dZ@�
=@��@��y@��y@�n�@�~�@��R@�^5@��D@���@�
=@�ff@���@�hs@���@��u@�b@���@��@��m@�ƨ@�ƨ@��F@�t�@�C�@��@���@�M�@�-@��@��-@���@��@�O�@�%@�7L@�&�@��@��@���@���@���@��@�z�@��u@�I�@�9X@� �@��;@��P@�dZ@�C�@���@��+@���@�=q@�V@�-@�@���@�x�@�X@�?}@�V@��j@�z�@�I�@�b@��;@��@�\)@�C�@��@�
=@�@��@��y@�ȴ@��!@�~�@�V@�-@�M�@�l�@�1@��@�I�@�j@�;d@�v�@�ȴ@�o@��@�ȴ@��\@��R@���@�ƨ@�Ĝ@��j@�(�@�|�@�;d@�"�@��H@��!@�M�@�J@��@���@�/@��F@�n�@�E�@��^@��@��^@���@���@��-@�@���G�O�@�@�`B@�@w�@h��@bJ@[t�@O�;@J^5@B�!@;dZ@4�@-�h@'
=@!��@�j@X@Z@X@E�@
�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ɺB
ɺB
ɺB
ɺB
ȴB
ɺB
ɺB
��B
��B
�
B
�B
�B
�B
�5B
�mB
��BBBPB�B�B(�B@�BN�BT�B[#BaHBy�B��B�^B�dBǮB�BB'�BG�BJ�BT�B^5BhsBu�B�+B�JB��B��B��B��B�^B��B��B�B�`B�mB�B��B��B��B��B�B�B��BB%B1B�B�#B��B�jB�B��B��B��B��B��B��B�uB�JB�7B�=B�Bx�Bv�Bv�Bx�Br�B^5BR�B6FB#�B�BJBB�B��BĜB�XB��B�\B�7Bs�BT�B>wB)�B
=B
�`B
�FB
��B
XB
)�B
�B	��B	�mB	�LB	��B	�=B	~�B	iyB	ZB	Q�B	G�B	=qB	)�B	�B	�B	�B	�B	hB	JB	%B	  B��B�B�B�sB�5B�
B��B��BB�^B�9B�B��B��B��B��B��B��B��B��B�oB�\B�PB�DB�7B�+B�B�B�B�B�B}�Bz�B{�B}�B|�B{�By�Bx�Bv�Bt�Bs�Bs�Bu�B~�B�JB�VB�hB��B��B��B�{B��B��B��B��B��B��B�XB�3B�B�B�!B�?B�9B�9B�FB�XB�qB�}B�}BÖBǮB��B��B��B��B�B�B�B�B�B�#B�BB�BB�BB�HB�`B�B�B�B�B�B�B�B��B��B��B	  B	B	B	B	B	%B		7B	PB	VB	
=B	B��B��B��B��B	  B	B	B		7B	+B	B��B��B��B��B��B��B��B��B��B	B	B	B	B	%B	DB	{B	�B	�B	�B	"�B	$�B	&�B	9XB	I�B	K�B	L�B	I�B	F�B	C�B	@�B	B�B	D�B	K�B	Q�B	VB	S�B	N�B	<jB	2-B	,B	1'B	1'B	49B	/B	+B	)�B	+B	+B	0!B	5?B	8RB	;dB	<jB	<jB	K�B	[#B	YB	Q�B	K�B	L�B	O�B	Q�B	Q�B	VB	bNB	dZB	ffB	jB	m�B	q�B	u�B	w�B	x�B	{�B	z�B	z�B	|�B	~�B	�B	�%B	�=B	�DB	�DB	�DB	�=B	�7B	�1B	�B	�%B	�PB	�VB	�DB	�B	{�B	q�B	p�B	p�B	p�B	q�B	q�B	p�B	p�B	q�B	r�B	t�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�7B	�DB	�JB	�VB	�VB	�oB	��B	��B	�{B	�oB	�hB	�hB	�hB	�hB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�?B	�LB	�RB	�RB	�XB	�XB	�RB	�XB	�dB	�jB	��B	��B	B	ÖB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�)B	�)B	�/B	�5B	�BB	�NB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
%B
	7B
�B
�B
)�B
33B
5?B
;dB
@�B
G�B
N�B
VB
[#B
`BB
gmB
k�B
iyB
p�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
ɸB
ɵB
ɷB
ɴB
ȰB
ɶB
ɶB
ʿB
��B
�B
�B
�B
�B
�0B
�kB
��B	BBMB�B�B(�B@}BN�BT�B[BaCBy�B��B�WB�^BǧB�B
B'�BG�BJ�BT�B^.BhlBu�B�%B�DB��B��B��B��B�YB��B��B�B�\B�fB�B��B��B��B��B�B�B��BBB-B�B�B��B�bB�B��B��B��B��B��B�zB�iB�DB�+B�3B�Bx�Bv�Bv�Bx�Br�B^.BR�B6@B#�ByBBBB�B��BēB�NB��B�TB�.Bs�BT�B>mB)�B
5B
�YB
�@B
�}B
XB
)�B
B	��B	�mB	�LB	��B	�@B	~�B	i{B	Z#B	Q�B	G�B	=vB	*B	�B	�B	�B	�B	oB	PB	-B	 B��B�B�B�{B�>B�B��B��BB�gB�AB�#B�B��B��B��B��B��B��B��B�|B�jB�[B�SB�DB�7B�+B�&B�B�B�B}�Bz�B{�B}�B|�B{�By�Bx�Bv�Bt�Bs�Bs�Bu�BB�TB�aB�vB��B��B��B��B��B��B��B��B��B��B�bB�:B�B�"B�)B�HB�?B�@B�NB�aB�xB��B��BÝBǲB��B��B��B�B�
B�B�#B�B�B�(B�IB�HB�IB�OB�fB�B�B�B�B�B�B�B��B��B��B	 B	B	B	B	 B	)B		<B	UB	ZB	
?B	!B��B��B��B��B	 B	B	B		<B	/B	B��B��B��B��B��B��B��B��B��B	B	#B	"B	#B	)B	IB	~B	�B	�B	�B	"�B	$�B	&�B	9YB	I�B	K�B	L�B	I�B	F�B	C�B	@�B	B�B	D�B	K�B	Q�B	VB	S�B	N�B	<lB	2.B	,
B	1)B	1(B	4;B	/B	+B	)�B	+B	+B	0"B	5BB	8UB	;fB	<kB	<jB	K�B	[$B	YB	Q�B	K�B	L�B	O�B	Q�B	Q�B	VB	bPB	dZB	ffB	j~B	m�B	q�B	u�B	w�B	x�B	{�B	z�B	z�B	|�B	~�B	�B	�!B	�<B	�DB	�DB	�AB	�>B	�5B	�1B	�B	�#B	�OB	�TB	�BB	�B	{�B	q�B	p�B	p�B	p�B	q�B	q�B	p�B	p�B	q�B	r�B	t�B	y�B	{�B	}�B	�B	�
B	�B	�B	�B	�B	�!B	�/B	�4B	�6B	�BB	�HB	�UB	�VB	�oB	��B	��B	�{B	�lB	�gB	�fB	�hB	�eB	�eB	�gB	�kB	�uB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�*B	�:B	�JB	�PB	�PB	�VB	�VB	�OB	�SB	�aB	�fB	��B	��B	B	ÓB	ŢB	ǬB	ǭB	ȰB	ɷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�&B	�)B	�.B	�@B	�JB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��G�O�B	��B
B
	2B
�B
�B
)�B
3/B
58B
;\B
@|B
G�B
N�B
U�B
[B
`;B
gfB
k{B
isB
p�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.09 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436402016080714364020160807143640  AO  ARCAADJP                                                                    20151013091557    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151013091557  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151013091557  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143640  IP                  G�O�G�O�G�O�                