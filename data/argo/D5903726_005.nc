CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:03:18Z AOML 3.0 creation; 2016-05-26T23:45:35Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230318  20160526164535  5903726 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4045_7089_005                   2C  D   APEX                            5372                            041511                          846 @�4�'q@1   @�4�VH �D���F@DU�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�3D�9�D��3D��3D�3D�P D�� D��3D���D�FfD�� Dǰ D���D�S3Dډ�D��fD��D�I�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B�#�B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�C�C�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D�zD�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�zDy��D�=D�8�D��=D��=D�=D�O
D�
D��=D���D�EpD��
Dǯ
D���D�R=Dڈ�D��pD��D�H�D�
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A��#A��;A��HA��TA��TA��HA��TA��TA��`A��`A��mA��yA��yA��yA��A��yA��TA��HA��mA��A��mA��mA��`A��TA��;A��9A��9A��A�G�A��RA��;A��A�bNA��A��PA���A��hA�C�A���A���A��mA���A�ZA��hA��A��^A�bNA��A�G�A�oA�JA�A���A���A�1'A���A�ffA�;dA�bA���A��HA���A��FA���A�5?A�%A���A��PA�Q�A�&�A�A��A��7A�dZA�E�A�  A��
A���A�p�A��A�ĜA�p�A�(�A��mA��
A��!A�~�A�+A��A�ȴA��A�C�A�
=A��HA��A�I�A�AXA~��A~ZA~�A}�mA}�-A|��A{p�A{S�A{oAz�`Az�Az�jAz��Az��AyƨAx�AxbAwO�AwC�Aw;dAv�Au�FAu�Au+At5?As�Ar�Aq�-Ap��Ao33An~�An$�AmO�AlE�Ak7LAj�HAj�+Ai�
Ah�HAhE�Ah�Ag�PAf��Ae��Ae��Ae�PAe��Ae�hAehsAeG�Ae
=AdffAcp�Ac33Ab �Aa`BA`�HA`��A`��A`ZA`1'A_ƨA_;dA^��A^�+A^{A]�#A]�FA]G�A\�jA[�TA[O�A[AZ��AZ�DAZVAZ-AY�-AYC�AX^5AXbNAX5?AW��AW�mAW��AWC�AV�\AU��AUG�AT��ATr�AT�AS�TARZAQXAPA�AO�;AO�^AOl�AN�ANn�AM�hAL��AL=qAK`BAJ��AJv�AH�AGG�AF��AF �AE��AE7LADĜAD�!AD��AD��AD�`AEVAD��AD1'AC��ACt�AC/AB��AA��AAhsAA�A@��A?�A?p�A>�A>ffA>JA=��A<�`A<jA;��A:v�A9�wA9t�A9;dA9�A8��A8�RA8  A7�7A7?}A6VA5hsA4ffA3�7A2��A25?A1��A1;dA0ĜA0�+A0VA/�A/�A/VA.�uA.$�A. �A-�mA-ƨA-ƨA-"�A,�uA,��A,�uA+��A*�A*�RA*��A*�A)VA(�RA(z�A'�A'�hA'+A&9XA%A%�A$�HA$�9A$E�A#��A#��A#?}A"�yA"�RA"�A"-A!"�A ��A �+A M�A E�A $�A A�mA�A��A��At�A&�A��A(�A��A|�AdZAK�A�yAE�A��AS�AA��A�A�+A-A"�A=qAG�A��Ar�A��A&�A�AbNAJA�-A�Al�AXA�A�9A=qA�A�FAS�A�\A^5AA�A�FA�A�\A  At�AS�A/A�A��A�!A�
AoA
�HA
��A
��A
ZA
$�A
1A	��A��A�jAA�A�Ap�AVAJA��A��AhsAA��A��A5?A�TA�-AXA�A�+A5?A�7A"�A �jA �9A �RA �9A jA 5?A {@�M�@��@�|�@��!@�ȴ@�ff@�@��/@�(�@�o@�@�@�
=@�@�E�@�p�@�r�@�1@�~�@���@�^5@�j@�b@畁@�^5@�z�@�
=@��T@�9@���@�`B@ۥ�@ڟ�@�&�@؋D@�b@׍P@�o@�v�@�?}@�bN@ӕ�@ѩ�@�1'@��@˕�@ɩ�@ȴ9@ǍP@�33@��@���@ēu@�Q�@�\)@�v�@�/@�z�@�\)@��^@�&�@�V@���@�r�@��!@��@���@�G�@�7L@�7L@�7L@��@�(�@�E�@�Ĝ@�1@���@���@�@�@��^@�x�@�7L@���@�Z@�ƨ@���@�V@���@�?}@��/@�I�@��9@�;d@��@��@�`B@�1'@v�+@mO�@e��@^��@V�+@PQ�@Ko@D�@>�y@;��@8A�@6ff@41@1�#@0��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A��#A��;A��HA��TA��TA��HA��TA��TA��`A��`A��mA��yA��yA��yA��A��yA��TA��HA��mA��A��mA��mA��`A��TA��;A��9A��9A��A�G�A��RA��;A��A�bNA��A��PA���A��hA�C�A���A���A��mA���A�ZA��hA��A��^A�bNA��A�G�A�oA�JA�A���A���A�1'A���A�ffA�;dA�bA���A��HA���A��FA���A�5?A�%A���A��PA�Q�A�&�A�A��A��7A�dZA�E�A�  A��
A���A�p�A��A�ĜA�p�A�(�A��mA��
A��!A�~�A�+A��A�ȴA��A�C�A�
=A��HA��A�I�A�AXA~��A~ZA~�A}�mA}�-A|��A{p�A{S�A{oAz�`Az�Az�jAz��Az��AyƨAx�AxbAwO�AwC�Aw;dAv�Au�FAu�Au+At5?As�Ar�Aq�-Ap��Ao33An~�An$�AmO�AlE�Ak7LAj�HAj�+Ai�
Ah�HAhE�Ah�Ag�PAf��Ae��Ae��Ae�PAe��Ae�hAehsAeG�Ae
=AdffAcp�Ac33Ab �Aa`BA`�HA`��A`��A`ZA`1'A_ƨA_;dA^��A^�+A^{A]�#A]�FA]G�A\�jA[�TA[O�A[AZ��AZ�DAZVAZ-AY�-AYC�AX^5AXbNAX5?AW��AW�mAW��AWC�AV�\AU��AUG�AT��ATr�AT�AS�TARZAQXAPA�AO�;AO�^AOl�AN�ANn�AM�hAL��AL=qAK`BAJ��AJv�AH�AGG�AF��AF �AE��AE7LADĜAD�!AD��AD��AD�`AEVAD��AD1'AC��ACt�AC/AB��AA��AAhsAA�A@��A?�A?p�A>�A>ffA>JA=��A<�`A<jA;��A:v�A9�wA9t�A9;dA9�A8��A8�RA8  A7�7A7?}A6VA5hsA4ffA3�7A2��A25?A1��A1;dA0ĜA0�+A0VA/�A/�A/VA.�uA.$�A. �A-�mA-ƨA-ƨA-"�A,�uA,��A,�uA+��A*�A*�RA*��A*�A)VA(�RA(z�A'�A'�hA'+A&9XA%A%�A$�HA$�9A$E�A#��A#��A#?}A"�yA"�RA"�A"-A!"�A ��A �+A M�A E�A $�A A�mA�A��A��At�A&�A��A(�A��A|�AdZAK�A�yAE�A��AS�AA��A�A�+A-A"�A=qAG�A��Ar�A��A&�A�AbNAJA�-A�Al�AXA�A�9A=qA�A�FAS�A�\A^5AA�A�FA�A�\A  At�AS�A/A�A��A�!A�
AoA
�HA
��A
��A
ZA
$�A
1A	��A��A�jAA�A�Ap�AVAJA��A��AhsAA��A��A5?A�TA�-AXA�A�+A5?A�7A"�A �jA �9A �RA �9A jA 5?A {@�M�@��@�|�@��!@�ȴ@�ff@�@��/@�(�@�o@�@�@�
=@�@�E�@�p�@�r�@�1@�~�@���@�^5@�j@�b@畁@�^5@�z�@�
=@��T@�9@���@�`B@ۥ�@ڟ�@�&�@؋D@�b@׍P@�o@�v�@�?}@�bN@ӕ�@ѩ�@�1'@��@˕�@ɩ�@ȴ9@ǍP@�33@��@���@ēu@�Q�@�\)@�v�@�/@�z�@�\)@��^@�&�@�V@���@�r�@��!@��@���@�G�@�7L@�7L@�7L@��@�(�@�E�@�Ĝ@�1@���@���@�@�@��^@�x�@�7L@���@�Z@�ƨ@���@�V@���@�?}@��/G�O�@��9@�;d@��@��@�`B@�1'@v�+@mO�@e��@^��@V�+@PQ�@Ko@D�@>�y@;��@8A�@6ff@41@1�#@0��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BhB
=BuB�B�B�B�B!�B)�B-B.B/B/B0!B49B5?B:^B=qB@�BA�BB�BE�BE�BE�BD�BC�BB�BA�BB�BB�BA�BA�B@�B@�B@�B?}B>wB>wB=qB=qB<jB:^B:^B8RB7LB6FB6FB5?B49B33B2-B/B,B)�B&�B#�B!�B �B �B�B�B�B�BoBVB
=B1B+BB  B��B��B�B�B�B��B�B�sB�mB�TB�HB�BB�;B�;B�;B�#B�B��B��B��B��B��BƨBĜBB�jB�RB�FB�B��B��B��B��B��B�bB�DB�7B�+B�B}�B{�Bz�Bw�Br�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bn�BjBdZBaHBXBP�BN�BN�BM�BN�BN�BJ�BF�BD�B@�B=qB:^B8RB49B.B'�B%�B$�B"�B"�B!�B�B�B�BJB\B\BVBPBJB	7BB��B��B��B�B�B�B�TB�B��B��B��BɺBŢBB�dB�3B�B��B��B��B�Bp�BiyBgmBbNB_;B`BBgmBjBjBk�Bn�Bp�Bl�BiyBffBcTB^5BVBQ�BN�BJ�BD�B?}B;dB7LB33B/B(�B#�B�BoBJB	7B+B%BBB��B��B�B�yB�BB�B��BɺBÖB�wB�XB�?B�3B�'B�B��B��B��B��B��B��B��B��B��B�oB�oB�oB�PB�1B�%B�+B� Bv�Bs�Bq�Bn�Bk�BhsBaHB^5B[#BVBS�BP�BL�BK�BH�BF�BD�BC�BA�B8RB6FB2-B1'B0!B0!B/B/B/B/B-B,B)�B&�B!�B�B�B�B�B�B{BbBPBDBDB
=B%BB
��B
�B
�mB
�TB
�;B
�B
��B
��B
��B
��B
ɺB
ǮB
ǮB
ƨB
ĜB
��B
�}B
�qB
�dB
�XB
�FB
�9B
�3B
�'B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�uB
�oB
�hB
�\B
�VB
�PB
�=B
�%B
�B
�B
~�B
{�B
v�B
u�B
s�B
r�B
p�B
o�B
m�B
l�B
jB
hsB
hsB
gmB
dZB
cTB
bNB
_;B
^5B
\)B
\)B
\)B
\)B
ZB
YB
W
B
R�B
N�B
L�B
J�B
I�B
H�B
F�B
C�B
@�B
<jB
8RB
49B
/B
.B
,B
)�B
&�B
$�B
�B
�B
�B
oB
hB
\B
JB
1B
B
B
  B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�sB	�fB	�TB	�BB	�#B	�B	��B	��B	ȴB	ƨB	ĜB	B	��B	�wB	�qB	�dB	�XB	�LB	�?B	�3B	�'B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�wB	��B	�B	��B
hB
 �B
6FB
I�B
[#B
n�B
�%B
��B
�B
�jB
��B
�;B
�111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BrG�O�B�B�B�B�B�B!�B*B-B."B/$B/)B0.B4EB5KB:mB=B@�BA�BB�BE�BE�BE�BD�BC�BB�BA�BB�BB�BA�BA�B@�B@�B@�B?�B>�B>�B=B=�B<vB:jB:kB8_B7[B6TB6RB5NB4GB3?B27B/(B,B*B&�B#�B!�B �B �B�B�B�B�BvBdB
LB:B9B#B 
B��B��B��B�B��B��B�B�|B�vB�aB�RB�JB�EB�GB�EB�0B�B��B��B��B��B��BƲBġBB�tB�^B�RB�B��B��B��B��B��B�iB�MB�>B�3B�B}�B{�Bz�Bw�Br�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bn�Bj�BddBaRBXBP�BN�BN�BM�BN�BN�BJ�BF�BD�B@�B=tB:cB8XB4AB.B'�B%�B$�B"�B"�B!�B�B�B�BOBcBcBZBTBQB	>BB��B��B��B�B�B�B�WB�"B��B��B��BɽBŨBB�jB�5B�B��B��B��B�!Bp�Bi~BgoBbSB_AB`BBgpBj�Bj�Bk�Bn�Bp�Bl�Bi}BfkBcYB^9BVBQ�BN�BJ�BD�B?�B;hB7OB36B/!B(�B#�B�BrBLB	9B,B&BBB��B��B�B�{B�FB�B��BɾBÙB�zB�[B�DB�4B�,B�B��B��B��B��B��B��B��B��B��B�tB�tB�tB�TB�4B�)B�.B�Bv�Bs�Bq�Bn�Bk�BhvBaLB^:B[(BVBS�BP�BL�BK�BH�BF�BD�BC�BA�B8XB6KB22B1,B0&B0%B/!B/!B/"B/B-B,B)�B&�B!�B�B�B�B�B�B�BgBQBHBJB
@B.BB
��B
�B
�uB
�ZB
�?B
�#B
�B
��B
��B
��B
��B
ǵB
ǴB
ƯB
ĤB
��B
��B
�wB
�jB
�`B
�OB
�@B
�;B
�-B
�B
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
�|B
�tB
�oB
�eB
�_B
�XB
�EB
�,B
�&B
�B
B
{�B
v�B
u�B
s�B
r�B
p�B
o�B
m�B
l�B
j�B
hyB
hxB
gtB
d`B
c]B
bXB
_BB
^=B
\3B
\2B
\1B
\2B
Z$B
YB
WB
R�B
N�B
L�B
J�B
I�B
H�B
F�B
C�B
@�B
<tB
8\B
4CB
/$B
.B
,B
*B
&�B
$�B
�B
�B
�B
yB
tB
cB
UB
<B
(B
B
 
B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�~B	�pB	�]B	�LB	�-B	�B	��B	��B	��B	ƴB	ĨB	B	��B	��B	�}B	�sB	�dB	�YB	�KB	�@B	�2B	�"B	�B	�B	�B	�	B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	��B	��B	�B	��B	�B	�B	�B
qB
 �B
6OB
I�B
[*B
n�B
�,B
��B
�B
�pB
��B
�BB
�111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605261645352016052616453520160526164535  AO  ARCAADJP                                                                    20140721230318    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230318  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230318  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160526164535  IP                  G�O�G�O�G�O�                