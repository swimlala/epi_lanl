CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:40Z AOML 3.0 creation; 2016-05-31T19:14:35Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230540  20160531121435  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               AA   AO  4051_7090_065                   2C  D   APEX                            5368                            041511                          846 @����~?�1   @���9� @4D���S��e$�/�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    AA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Doy�Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyL�D�	�D�C3D��fD�ٚD�3D�6fD�s3D�� D���D�FfD�i�D��3D�	�D�FfDڙ�D�ɚD�  D�L�D� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��qB��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCR�CS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dgw�Dg��Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn��Dow�Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dtw�DyJ�D��D�B=D��pD�ؤD�=D�5pD�r=D��
D���D�EpD�h�D��=D��D�EpDژ�D�ȤD��
D�K�D�
D��p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��ÃȦ+Ḁ�A�~�A��A���A˥�A�7LA��A��A��A�VA���A��;AʋDA�jA�dZA�Q�A�7LA�+A�(�A�(�A�&�A��A�
=A���A��TA���Aɝ�AɋDA�1'A���AȃA�M�A��A��
Aǉ7A�l�A�\)A�G�A� �A�VA���A�9XA�VA�ƨA�A�VA�jA�A�A���A���A���A�A��HA��A�ffA��A�z�A���A�JA��+A�hsA���A��7A���A�C�A���A���A�1A�`BA�"�A�p�A�7LA�{A��`A�n�A�A�1A��TA�l�A�7LA�|�A�-A�ȴA��!A��hA��PA��A�(�A���A��A�bA��FA��/A�v�A�?}A�l�A�Q�A���A��+A�{A�|�A�S�A�ƨA���A���A��A�$�A���A�ȴAG�A}&�A|E�A|  A{�A{�
A{G�Ay�Aw|�AvAt��At$�As`BAr�9AqdZAp��Ao��An�+Al�jAk%Ag�;AeS�Aa�wA]&�A[�A[&�AZ�AY��AW��AV�AU+AQ?}APn�AOx�AN��AM�#AL�AI��AE�AE?}AEoAD�\AC?}AAAA?}A>9XA=G�A<A�A;7LA:��A9�A8ZA8(�A8�A7��A5��A4bNA4JA37LA0jA.��A-�^A-��A-7LA)ƨA&��A%��A$�A#��A"�\A!\)A �+A��A�`AO�AO�AȴA�wA�DAAO�A��AM�A��A�AVA5?AƨA�A��A�jA�!A�uA^5Ap�AO�A�At�A
��A	��A	hsA	O�A	C�A	?}A	33A	+A	%A�A��A�jAr�A�#A~�AG�A  A�-AĜA�A�A ^5@�X@���@�-@�V@��9@�bN@���@�
=@�ȴ@�V@��7@��`@�(�@��@�h@�(�@��@��@�(�@�33@�R@�=q@�^@��@�o@�5?@��u@�V@ە�@�\)@�S�@�33@���@�1@�{@�bN@�5?@�%@мj@Ь@�9X@�ƨ@υ@��H@��@�x�@�?}@�z�@��@�|�@ʇ+@�7L@�Q�@Ǯ@�
=@�v�@�=q@Ų-@���@ļj@�b@�n�@���@�"�@�"�@��R@�J@�7L@�  @��\@��@���@��-@���@�p�@�hs@��@��@�Ĝ@�A�@���@��+@���@�p�@�Z@�"�@��#@�G�@��@��j@��D@�A�@��h@�Q�@�1@���@�C�@���@�=q@��T@�p�@��j@��@��F@�l�@�o@�-@��@�j@�bN@�Z@�I�@�1'@� �@��m@���@�|�@�t�@�dZ@�\)@�S�@�K�@�K�@�33@�+@�"�@�o@���@��@���@�V@���@��h@�hs@�V@��j@�(�@�|�@�K�@�;d@�+@�"�@��@�
=@��@��@���@�E�@��@��T@���@�x�@�7L@�%@��j@�z�@�r�@�bN@�Q�@�1'@��@�  @��
@���@��@�33@���@���@���@�@���@�`B@���@��@�z�@�r�@�I�@�(�@�b@���@��w@��@�;d@�
=@�
=@���@��@��R@���@��\@�~�@�V@��@��7@��@��@�Z@�1@��w@��@�K�@��!@�V@�$�@��@���@��-@���@�hs@��j@�(�@���@�\)@�K�@�"�@��@�+@��@�o@�
=@��@��R@���@��\@�~�@�^5@�$�@��@��h@�&�@���@��/@���@��9@��D@�Z@�I�@� �@�1@���@��@�ȴ@���@��+@�n�@�V@�{@�?}@���@���@�I�@�A�@�  @���@��;@��
@��;@�^5@�7L@x�`@kƨ@e��@`�9@Z=q@P  @I�#@B��@=��@5�@1�#@+��@%�@"M�@�#@V@l�@
�@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ÃȦ+Ḁ�A�~�A��A���A˥�A�7LA��A��A��A�VA���A��;AʋDA�jA�dZA�Q�A�7LA�+A�(�A�(�A�&�A��A�
=A���A��TA���Aɝ�AɋDA�1'A���AȃA�M�A��A��
Aǉ7A�l�A�\)A�G�A� �A�VA���A�9XA�VA�ƨA�A�VA�jA�A�A���A���A���A�A��HA��A�ffA��A�z�A���A�JA��+A�hsA���A��7A���A�C�A���A���A�1A�`BA�"�A�p�A�7LA�{A��`A�n�A�A�1A��TA�l�A�7LA�|�A�-A�ȴA��!A��hA��PA��A�(�A���A��A�bA��FA��/A�v�A�?}A�l�A�Q�A���A��+A�{A�|�A�S�A�ƨA���A���A��A�$�A���A�ȴAG�A}&�A|E�A|  A{�A{�
A{G�Ay�Aw|�AvAt��At$�As`BAr�9AqdZAp��Ao��An�+Al�jAk%Ag�;AeS�Aa�wA]&�A[�A[&�AZ�AY��AW��AV�AU+AQ?}APn�AOx�AN��AM�#AL�AI��AE�AE?}AEoAD�\AC?}AAAA?}A>9XA=G�A<A�A;7LA:��A9�A8ZA8(�A8�A7��A5��A4bNA4JA37LA0jA.��A-�^A-��A-7LA)ƨA&��A%��A$�A#��A"�\A!\)A �+A��A�`AO�AO�AȴA�wA�DAAO�A��AM�A��A�AVA5?AƨA�A��A�jA�!A�uA^5Ap�AO�A�At�A
��A	��A	hsA	O�A	C�A	?}A	33A	+A	%A�A��A�jAr�A�#A~�AG�A  A�-AĜA�A�A ^5@�X@���@�-@�V@��9@�bN@���@�
=@�ȴ@�V@��7@��`@�(�@��@�h@�(�@��@��@�(�@�33@�R@�=q@�^@��@�o@�5?@��u@�V@ە�@�\)@�S�@�33@���@�1@�{@�bN@�5?@�%@мj@Ь@�9X@�ƨ@υ@��H@��@�x�@�?}@�z�@��@�|�@ʇ+@�7L@�Q�@Ǯ@�
=@�v�@�=q@Ų-@���@ļj@�b@�n�@���@�"�@�"�@��R@�J@�7L@�  @��\@��@���@��-@���@�p�@�hs@��@��@�Ĝ@�A�@���@��+@���@�p�@�Z@�"�@��#@�G�@��@��j@��D@�A�@��h@�Q�@�1@���@�C�@���@�=q@��T@�p�@��j@��@��F@�l�@�o@�-@��@�j@�bN@�Z@�I�@�1'@� �@��m@���@�|�@�t�@�dZ@�\)@�S�@�K�@�K�@�33@�+@�"�@�o@���@��@���@�V@���@��h@�hs@�V@��j@�(�@�|�@�K�@�;d@�+@�"�@��@�
=@��@��@���@�E�@��@��T@���@�x�@�7L@�%@��j@�z�@�r�@�bN@�Q�@�1'@��@�  @��
@���@��@�33@���@���@���@�@���@�`B@���@��@�z�@�r�@�I�@�(�@�b@���@��w@��@�;d@�
=@�
=@���@��@��R@���@��\@�~�@�V@��@��7@��@��@�Z@�1@��w@��@�K�@��!@�V@�$�@��@���@��-@���@�hs@��j@�(�@���@�\)@�K�@�"�@��@�+@��@�o@�
=@��@��R@���@��\@�~�@�^5@�$�@��@��h@�&�@���@��/@���@��9@��D@�Z@�I�@� �@�1@���@��@�ȴ@���@��+@�n�@�V@�{@�?}@���@���@�I�@�A�@�  @���@��;@��
@��;@�^5@�7L@x�`@kƨ@e��@`�9@Z=q@P  @I�#@B��@=��@5�@1�#@+��@%�@"M�@�#@V@l�@
�@?}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB=qB@�Bn�B�wB��B��BĜB��B��B��B��B��B�B�)B�HB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��BB	7B\B\BoB�B�B�B"�B/B/BuB�B�`B��B��B�B�B��B��B��B��B��B�=B_;BI�BF�B@�B8RB,B�BhBJB%B  B��B�B�5B�B��BɺBƨBĜB��B�XB�B�JBr�BiyBdZBW
BE�B@�B?}B>wB>wB=qB7LB0!B,B#�BPB��B�NB�9B��B��B�oBbNBG�B!�BB
��B
��B
�ZB
�}B
�!B
��B
q�B
W
B
H�B
C�B
A�B
@�B
?}B
:^B
,B
!�B
�B
hB
PB
	7B
B	��B	��B	�B	�sB	�5B	��B	��B	�'B	��B	�%B	}�B	{�B	x�B	r�B	k�B	e`B	]/B	L�B	G�B	B�B	>wB	;dB	49B	(�B	�B	�B	�B	oB	PB		7B	1B	B	  B��B��B��B�B�mB�fB�`B�NB�/B�#B�B��B��B��BǮBƨB��B�RB�3B�'B�B��B��B��B��B��B��B��B�oB�bB�VB�DB�1B�%B�B�B�B� B~�B}�B|�B{�B{�B{�B{�Bz�Bx�Bw�Bs�Bl�BjBhsBhsBhsBhsBhsBhsBhsBgmBgmBgmBgmBffBe`BcTBbNB`BB`BB^5B]/B]/B\)BZB[#BZBZB[#BZBZBZBZBZBZBZBYBYBYBYBXBYBYBZB[#B[#B]/B^5B]/BbNBcTBdZBbNBaHBaHB`BBaHBaHBaHBbNBffBjBl�Bl�Bk�Bl�Bm�Bm�Bn�Bp�Bq�Br�Bt�Bt�Bu�Bw�B|�B� B�B�B�%B�%B�1B�=B�=B�JB�oB��B��B��B�B�3B�dBŢB��B��B�B�#B�;B�5B�5B�5B�TB�fB�B�B�B�B�B��B	B		7B	JB	PB	VB	VB	VB	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	(�B	,B	-B	.B	0!B	49B	9XB	=qB	>wB	>wB	>wB	?}B	?}B	A�B	D�B	E�B	E�B	F�B	F�B	G�B	G�B	G�B	H�B	H�B	H�B	I�B	K�B	L�B	M�B	R�B	YB	\)B	^5B	cTB	gmB	n�B	v�B	y�B	y�B	z�B	z�B	{�B	|�B	~�B	�B	�B	�=B	�PB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�3B	�?B	�FB	�LB	�LB	�XB	�^B	�dB	�jB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�5B	�;B	�BB	�HB	�NB	�NB	�NB	�TB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
B
\B
�B
#�B
(�B
-B
2-B
9XB
?}B
E�B
J�B
O�B
S�B
ZB
_;B
bNB
hsB
n�B
s�B
w�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B=uB@�Bn�B��B��B��BģB��B��B��B��B�B�"B�4B�UB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��BB	@BiBgB{B�B�B�B"�B/*B/)B�B�B�kB��B��B�*B�"B�B��B��B��B��B�DB_ABI�BF�B@�B8XB,B�BlBNB)B B��B�B�6B�B��BɿBƮBĢB��B�[B�B�NBr�BizBdYBWBE�B@�B?~B>|B>|B=uB7OB0$B,B#�BPB��B�QB�<B��B��B�rBbTBG�B!�BB
��B
��B
�_B
��B
�)B
��B
q�B
WB
H�B
C�B
A�B
@�B
?�B
:gB
,B
!�B
�B
sB
YB
	?B
'B	��B	��B	�B	�B	�BB	��B	��B	�4B	��B	�5B	~B	{�B	x�B	r�B	k�B	epB	]?B	L�B	G�B	B�B	>�B	;vB	4IB	)
B	�B	�B	�B	�B	fB		MB	EB	$B	 B�B��B��B�B�B�}B�rB�dB�AB�7B�&B�B��B��B��BƾB��B�iB�IB�@B�,B�B��B��B��B��B��B��B��B�{B�nB�\B�HB�?B�2B�&B� B�BB~B}B| B| B| B|Bz�Bx�Bw�Bs�Bl�Bj�Bh�Bh�Bh�Bh�Bh�Bh�Bh�Bg�Bg�Bg�Bg�Bf�Be{BcoBbhB`]B`\B^QB]JB]HB\EBZ7B[?BZ:BZ8B[?BZ6BZ6BZ7BZ8BZ6BZ9BZ:BY1BY3BY1BY1BX,BY0BY1BZ6B[<B[>B]JB^MB]HBbhBcnBduBbhBadBacB`[BacBacBabBbeBf~Bj�Bl�Bl�Bk�Bl�Bm�Bm�Bn�Bp�Bq�Br�Bt�Bt�Bu�Bw�B}B�B�$B�2B�>B�=B�IB�VB�SB�aB��B��B��B� B�B�IB�yBŷB��B�B�&B�5B�PB�JB�JB�JB�hB�zB�B�B�B��B��B��B	 B		KB	]B	aB	kB	iB	iB	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	)B	,B	-B	.%B	04B	4JB	9hB	=�B	>�B	>�B	>�B	?�B	?�B	A�B	D�B	E�B	E�B	F�B	F�B	G�B	G�B	G�B	H�B	H�B	H�B	I�B	K�B	L�B	M�B	SB	Y)B	\:B	^EB	cdB	g{B	n�B	v�B	y�B	y�B	z�B	z�B	{�B	|�B	B	�B	�(B	�NB	�\B	�iB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�.B	�3B	�AB	�LB	�RB	�WB	�WB	�eB	�lB	�rB	�vB	��B	��B	��B	B	âB	ĪB	ƴB	ǻB	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�(B	�.B	�BB	�GB	�MB	�SB	�XB	�YB	�WB	�`B	�qB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B
 B
 B	�B	�B
$B
hB
�B
#�B
) B
-B
28B
9aB
?�B
E�B
J�B
O�B
T B
Z$B
_BB
bWB
hzB
n�B
s�B
w�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214352016053112143520160531121435  AO  ARCAADJP                                                                    20140721230540    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230540  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230540  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121435  IP                  G�O�G�O�G�O�                