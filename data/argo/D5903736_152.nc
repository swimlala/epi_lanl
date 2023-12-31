CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:51Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041151  20190604094021  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @׶����1   @׶�4&f@2��Q��d.=p��
1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�D�A�D�nD�ָD��=D�:=D�h D�ƸD�fD�B�D���D��)D��D�4)Dڏ
D��HD��D�D{D�qD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB(G�B/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB�#�B�#�B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	�zD	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm�zDm�Dn�zDn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt��Dy��D�)D�@�D�mD���D��GD�9GD�g
D���D�pD�A�D�� D��3D��D�33DڎD��RD��D�C�D�{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A�33A�/A�/A�-A�bA�
=A�1A�A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�%A�%A�JA��A�?}A׃Aװ!A�|�A�G�A֩�A�z�A�K�AҲ-A�C�A�K�Aѝ�A�A��A�r�A��HA��ÃA̡�A�C�A�p�A�n�Aʝ�A�VA�$�A�A�A�A�p�A���AŰ!Aŕ�A�ZA���Aú^A�I�A���A��#A�dZA���A��jA�G�A���A�oA��A�K�A�A��
A�ĜA�A�A���A���A�;dA�ZA��A���A���A���A��yA��A�(�A��wA�
=A��wA��7A�1A���A���A�r�A��DA���A��mA���A�|�A��TA��A��hA��TA�n�A��#A��DA�VA�n�A�?}A�A�A�(�A��HA��#A�ƨA�?}A�~�A�hsA~M�A{��Az�jAz��Az�Aup�Ap��Ao?}AnVAm��Ak��AhȴAd�Ab �A_`BA]t�AZ�HAX�9AT��ASƨAR��AR��AQG�AOXANz�AL��AJ�\AH�HADE�A@9XA>Q�A<n�A9�hA8-A7dZA6��A6{A5��A5%A3��A3
=A2�DA1�A/��A.ĜA-�wA-�A,ZA+�;A+��A+S�A*�A*�DA)`BA(M�A'33A%C�A$�yA%K�A%XA$��A#�FA"�A!hsA �HA �AK�Az�Av�AƨA��A��AI�A��A��A"�At�A��A|�A{A��AƨAXA��A�`A�A~�Ax�A
ĜA
9XA	��A	G�A�AA��A�^AQ�A~�AO�A ��A ��A z�@���@���@�$�@��@�`B@�b@��@�=q@�r�@�@��@��@�ƨ@�;d@���@�@��@�S�@�v�@�z�@��@�$�@�V@�K�@��#@���@ߝ�@�v�@��
@�\)@ڟ�@��@�  @�|�@�=q@��@Դ9@�I�@�|�@�S�@ҏ\@щ7@� �@υ@�C�@���@Ώ\@�@�%@˥�@ɲ-@�ƨ@�@��@�%@ļj@�r�@��@Õ�@���@°!@���@���@�+@���@�G�@�/@��@��@�1@��
@�t�@�V@�p�@���@��m@�C�@��y@��+@�@��h@��@�bN@�K�@��R@���@�V@���@�Z@�  @��w@�\)@�ȴ@��+@�V@�=q@�J@�x�@���@��u@�9X@���@�1@��m@�S�@���@�&�@��@�1'@��@�
=@��@���@�E�@��h@�?}@�V@���@���@�Z@�I�@�1@��@�\)@���@���@�^5@���@�?}@��/@�Ĝ@��D@�b@�ƨ@�l�@�S�@�33@�o@�
=@��H@�M�@��#@��@�G�@�V@���@��u@�j@�I�@��@��w@���@�dZ@�"�@�@��y@��!@�n�@��@�p�@�`B@�`B@�`B@�O�@�?}@�&�@�%@��@��@��;@���@�|�@�dZ@�33@�o@���@��R@��!@���@���@��+@�ff@�@���@�(�@��@�\)@�;d@�
=@��@��\@�E�@�{@��T@���@�`B@�&�@���@��/@�Ĝ@�r�@�b@��
@��w@���@��P@�l�@�;d@��@���@���@�v�@�5?@�5?@�-@�{@��#@��-@���@�G�@���@��9@��@���@��u@�j@���@��@���@�1@�1@��@�b@��F@�t�@�;d@�
=@��@��@��!@���@�ff@�^5@�5?@�{@���@���@�x�@�X@�G�@�7L@�&�@�%@���@���@��@�j@�I�@�9X@��
@�
=@�~�@�^5@�5?@��^@�x�@�`B@�`B@��@}��@t]d@j�,@^�M@V�c@Ob�@H�@A��@9�>@2)�@,6@%��@!�@��@�M@��@��@O�@	zx@�n111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�33A�33A�/A�/A�-A�bA�
=A�1A�A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�%A�%A�JA��A�?}A׃Aװ!A�|�A�G�A֩�A�z�A�K�AҲ-A�C�A�K�Aѝ�A�A��A�r�A��HA��ÃA̡�A�C�A�p�A�n�Aʝ�A�VA�$�A�A�A�A�p�A���AŰ!Aŕ�A�ZA���Aú^A�I�A���A��#A�dZA���A��jA�G�A���A�oA��A�K�A�A��
A�ĜA�A�A���A���A�;dA�ZA��A���A���A���A��yA��A�(�A��wA�
=A��wA��7A�1A���A���A�r�A��DA���A��mA���A�|�A��TA��A��hA��TA�n�A��#A��DA�VA�n�A�?}A�A�A�(�A��HA��#A�ƨA�?}A�~�A�hsA~M�A{��Az�jAz��Az�Aup�Ap��Ao?}AnVAm��Ak��AhȴAd�Ab �A_`BA]t�AZ�HAX�9AT��ASƨAR��AR��AQG�AOXANz�AL��AJ�\AH�HADE�A@9XA>Q�A<n�A9�hA8-A7dZA6��A6{A5��A5%A3��A3
=A2�DA1�A/��A.ĜA-�wA-�A,ZA+�;A+��A+S�A*�A*�DA)`BA(M�A'33A%C�A$�yA%K�A%XA$��A#�FA"�A!hsA �HA �AK�Az�Av�AƨA��A��AI�A��A��A"�At�A��A|�A{A��AƨAXA��A�`A�A~�Ax�A
ĜA
9XA	��A	G�A�AA��A�^AQ�A~�AO�A ��A ��A z�@���@���@�$�@��@�`B@�b@��@�=q@�r�@�@��@��@�ƨ@�;d@���@�@��@�S�@�v�@�z�@��@�$�@�V@�K�@��#@���@ߝ�@�v�@��
@�\)@ڟ�@��@�  @�|�@�=q@��@Դ9@�I�@�|�@�S�@ҏ\@щ7@� �@υ@�C�@���@Ώ\@�@�%@˥�@ɲ-@�ƨ@�@��@�%@ļj@�r�@��@Õ�@���@°!@���@���@�+@���@�G�@�/@��@��@�1@��
@�t�@�V@�p�@���@��m@�C�@��y@��+@�@��h@��@�bN@�K�@��R@���@�V@���@�Z@�  @��w@�\)@�ȴ@��+@�V@�=q@�J@�x�@���@��u@�9X@���@�1@��m@�S�@���@�&�@��@�1'@��@�
=@��@���@�E�@��h@�?}@�V@���@���@�Z@�I�@�1@��@�\)@���@���@�^5@���@�?}@��/@�Ĝ@��D@�b@�ƨ@�l�@�S�@�33@�o@�
=@��H@�M�@��#@��@�G�@�V@���@��u@�j@�I�@��@��w@���@�dZ@�"�@�@��y@��!@�n�@��@�p�@�`B@�`B@�`B@�O�@�?}@�&�@�%@��@��@��;@���@�|�@�dZ@�33@�o@���@��R@��!@���@���@��+@�ff@�@���@�(�@��@�\)@�;d@�
=@��@��\@�E�@�{@��T@���@�`B@�&�@���@��/@�Ĝ@�r�@�b@��
@��w@���@��P@�l�@�;d@��@���@���@�v�@�5?@�5?@�-@�{@��#@��-@���@�G�@���@��9@��@���@��u@�j@���@��@���@�1@�1@��@�b@��F@�t�@�;d@�
=@��@��@��!@���@�ff@�^5@�5?@�{@���@���@�x�@�X@�G�@�7L@�&�@�%@���@���@��@�j@�I�@�9X@��
@�
=@�~�@�^5@�5?@��^@�x�@�`BG�O�@��@}��@t]d@j�,@^�M@V�c@Ob�@H�@A��@9�>@2)�@,6@%��@!�@��@�M@��@��@O�@	zx@�n111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
l�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
n�B
n�B
o�B
p�B
q�B
q�B
r�B
s�B
s�B
t�B
u�B
u�B
u�B
v�B
v�B
w�B
z�B
�B
�uB
�FB
�NB
��BBuBoB0!B49B>wBQ�B]/BH�BD�BC�B��BɺB��B	7B"�B33B6FBO�BN�BI�BYB^5Bq�Bz�B~�B�1B��B��B�BÖB��B�#B�/B�BB��B+B��BBVB�B{BbBPB�B�BuBoBuBbB�B�BbBPBB��B�sB�B�NB��B�jB�-B�FB�9B��Bt�BR�B/B�BbBB�B�B�RB�-B�B�+B8RBhB
��B
�B
�fB
ƨB
�RB
��B
�bB
y�B
^5B
I�B
9XB
33B
1'B
)�B
DB	�NB	��B	ǮB	�}B	�'B	��B	�=B	{�B	n�B	dZB	W
B	J�B	;dB	49B	0!B	,B	$�B	�B	�B	\B	%B��B�B�`B�TB�)B�B�B�5B�5B�)B�B�#B�#B�B�
B�B�
B�B�B�/B�/B�5B�5B�5B�5B�/B�/B�B�B�B�B�yB�B��B��B�B�B�B�B�B�B�B�B�B�/B�)B�B��B��B��BɺBǮBÖB��B�wB�wB�qB�qB��B��B�wB�jB�^B�XB�LB�3B��B��B��B�DB�B�B� B~�B}�B{�Bz�Bx�Bv�Bs�Br�Bq�Br�Bs�Br�Bs�Bt�Bs�Br�Bt�Bx�Bx�Bx�Bw�Bw�Bw�Bx�Bz�B}�B�B�B� B� B�B�B�B�B�%B�%B�+B�7B�=B�=B�DB�DB�PB�bB�uB�{B�{B��B��B��B��B��B��B��B��B�B�!B�!B�'B�3B�3B�?B�FB�RB�wB�}BÖBŢBƨBƨBɺB��B��B��B��B��B�B�#B�5B�HB�TB�`B�sB�B�B�B��B��B��B	B	+B	JB	\B	�B	�B	�B	�B	 �B	 �B	"�B	(�B	+B	.B	1'B	33B	49B	8RB	>wB	@�B	A�B	A�B	E�B	I�B	J�B	K�B	O�B	VB	W
B	YB	[#B	\)B	_;B	aHB	bNB	cTB	ffB	hsB	jB	m�B	r�B	t�B	y�B	{�B	}�B	�B	�B	�%B	�%B	�%B	�1B	�1B	�DB	�PB	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�?B	�FB	�RB	�XB	�dB	�jB	�qB	�wB	�wB	�wB	�}B	�}B	�}B	�wB	�qB	�qB	��B	��B	��B	��B	B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�)B	�/B	�/B	�;B	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	�B
�B
 �B
'RB
1�B
6`B
=VB
DMB
J�B
UMB
]�B
c:B
iDB
l�B
qB
u�B
x�B
|�B
}B
�B
�z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
k^B
kZB
kWB
kZB
kXB
lbB
mdB
mhB
niB
mkB
mhB
nmB
orB
pxB
pyB
qB
r�B
r�B
s�B
t�B
t�B
t�B
u�B
u�B
v�B
y�B
��B
�EB
�B
�B
��B�BAB>B.�B3B=EBP�B[�BG}BCkBBeB��BȅB��B B!�B1�B5BN�BM�BH�BW�B] BpuBy�B}�B��B�TB��B��B�^BήB��B��B�B��B�B��B �B"BtB@B*BBnBlB=B7B<B&BpBnB0BB �B��B�EB�KB�B��B�3B��B�B�B�LBs�BQ�B-�BTB*B�B�_B��B�!B��B��B��B7 B;B
��B
�tB
�4B
�vB
�"B
��B
�8B
x�B
]B
H�B
8,B
2B
/�B
(�B

B	�B	ϺB	ƅB	�RB	��B	��B	�B	z�B	mjB	c/B	U�B	I�B	:9B	3B	.�B	*�B	#�B	}B	^B	2B	�B��B�_B�6B�&B��B��B��B�B�
B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�
B�B�B��B��B��B��B�PB�B�B��B�B��B�tB�nB�uB�kB�rB�B�XB�B��B��BϾBͳB˧BȑBƇB�lB�\B�NB�NB�KB�HB�cB�\B�RB�CB�3B�2B�!B�B��B��B�fB�B��B�B~�B}�B|�Bz�By�Bw�Bu�Br�Bq�Bp�Bq�Br�Bq�Br�Bs�Br�Bq�Bs�Bw�Bw�Bw�Bv�Bv�Bv�Bw�By�B|�B��B�B~�B~�B��B��B��B��B��B��B�B�B�B�B�B�B�'B�5B�PB�TB�TB�[B�YB�]B�sB��B��B��B��B��B��B��B��B�B�B�B�!B�.B�NB�WB�lB�yBńBŁBȔB̫BͳBθB��B��B��B��B�B�!B�.B�4B�KB�ZB�zB��B�B��B��B	�B	B	"B	5B	^B	�B	�B	�B	�B	�B	!�B	'�B	)�B	,�B	0 B	2B	3B	7-B	=OB	?XB	@`B	@`B	DxB	H�B	I�B	J�B	N�B	T�B	U�B	W�B	Y�B	[B	^B	`B	a%B	b/B	e=B	gKB	iTB	lkB	q�B	s�B	x�B	z�B	|�B	��B	��B	��B	��B	��B	�	B	�B	�B	�(B	�5B	�@B	�GB	�EB	�QB	�\B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�*B	�2B	�<B	�BB	�HB	�MB	�NB	�NB	�UB	�RB	�SB	�RB	�JB	�FB	�[B	�\B	�^B	�`B	�eB	�tB	�}B	ƇB	əB	ʝB	ˢB	ͯB	δB	γB	ϹB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	� B	�B	�B	�$B	�)B	�*B	�6B	�;B	�DB	�GB	�XB	�YB	�dB	�nB	�{B	�~B	�~B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
B
G�O�B
�B
kB
�B
&&B
0�B
56B
</B
C!B
I�B
T$B
\oB
bB
hB
k�B
o�B
t~B
w�B
{uB
~TB
��B
�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =-0.001(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940212019060409402120190604094021  AO  ARCAADJP                                                                    20181121041151    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041151  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041151  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094021  IP                  G�O�G�O�G�O�                