CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:35Z AOML 3.0 creation; 2016-05-31T19:14:33Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230535  20160531121433  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               7A   AO  4051_7090_055                   2C  D   APEX                            5368                            041511                          846 @־_�y`1   @־`Pg?�@5"I�^5�e.fffff1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    7A   B   B   @���@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B��B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�Y�D��3D���D��D�6fD��fD���D�� D�L�D�|�D��fD��D�9�D�i�D���D� D�I�D� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@�
=A�A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB`G�Bg�HBo�HBw�HBz�B��qB��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�C�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV��DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt��Dy��D��D�X�D��=D���D��D�5pD��pD���D��
D�K�D�{�D��pD��D�8�D�h�D���D�
D�H�D�
D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aܲ-AܸRAܶFAܼjA���A�ƨA�ƨA���A���A���A���A���A���A��#A��AܾwAܶFAܶFAܺ^Aܧ�A܁A�hsA�33A�
=A۩�AڑhA�|�AօA�|�A�hsA�hsA�9XAA��PA��/A�{A�ĜA���A�1'A�C�A�K�A�l�A�XA���A�\)A�|�A��jA�+A��HA�n�A�1A���A��A�l�A� �A��9A��FA�?}A�;dA�  A�S�A�E�A�7LA�7LA��A�r�A��A���A�G�A��A�Q�A���A���A�
=A�ȴA��hA��A�/A��A�|�A��A���A��A�r�A��uA�/A��
A�K�A�I�A�A�A���A��uA��jA�S�A��A��!A�VA���A�/A�x�A��A���A�  A���A��+A��A~$�A|�A{
=AzVAw��Av(�Au"�Ar�jApbAm�mAlA�AjZAh�jAf�AeS�Ac��AaS�A`r�A_oA^M�A^ �A\��AZn�AW�AV�9AVr�AVjAU��AT��AS��APĜAO�hAN��AM+AKS�AJZAIoAHffAHbAG��AF�AFI�AF9XAD�+AC?}AB��ABA�A<~�A:ffA8��A89XA7��A7t�A7VA5�;A4�DA3O�A1�^A0�A/VA.1A,�yA,-A+�#A+�^A+|�A+S�A*�RA*A)x�A'��A&�A&n�A%��A#�A"�A!��A �`A �\A+A�AO�A��AQ�A��A�7Ax�AG�A
=A��A��A"�A1'A�yA�jA$�A�wA�jAbA�A�;A�RA�;A
�HA
ffA
  A�RA�A�!A7LA1'AhsA Ĝ@���@�v�@��-@��@��@�o@��@�%@��@�ƨ@�{@��@� �@�/@���@߮@�S�@�$�@��#@�/@���@��@ە�@��@��@�33@��@�j@Ӆ@�"�@�-@�?}@�V@д9@�1@�C�@�^5@��@ͺ^@�p�@�Ĝ@��@���@��m@���@�t�@�"�@�@��H@ʧ�@�=q@ɺ^@�j@ǥ�@�\)@��H@�^5@�5?@�J@��T@Ł@�&�@��@°!@���@��j@�I�@�o@��^@�z�@���@���@�(�@��@��y@���@�hs@��w@��@���@��^@��@�r�@�r�@�Z@��w@�S�@�@��!@��\@�~�@�M�@��u@�9X@�1'@�9X@�1'@�(�@���@��R@�V@�$�@�@���@���@���@���@�x�@�&�@��@�Q�@�1'@�"�@�v�@���@�x�@�&�@�A�@��m@��
@�ƨ@���@��P@�t�@�\)@�C�@�+@�o@�@�ȴ@��+@�V@�-@���@�?}@��/@��9@�Ĝ@��j@��@��@�+@�
=@�ȴ@�v�@�M�@�@�hs@�%@��@�Ĝ@�b@�1@���@���@���@�l�@��@�=q@���@��^@��@�Z@�9X@�A�@�9X@��@��@�b@�1@�  @� �@�9X@�I�@�I�@�9X@�I�@�(�@��m@���@��F@���@��P@��P@��P@��@�|�@�K�@�
=@�
=@���@��@��!@���@���@���@��\@��+@�^5@�M�@�M�@�-@�J@��^@�V@���@�I�@�1'@� �@�b@��@�dZ@��y@��!@�n�@�M�@���@���@��h@�X@���@���@��@�I�@��@�+@�v�@�=q@�{@��#@��^@��^@���@��7@��7@�x�@�?}@�/@��@�b@��;@��
@�|�@��+@���@�x�@�hs@�%@���@��j@��9@��@�r�@�Z@�(�@��@��w@��@���@�o@�=q@��@���@��@�hs@�G�@�/@��@��@��@�"�@|�@p��@hr�@^@XbN@P�@M�@E@@  @8�@3��@1%@+�
@&5?@\)@��@{@  @	��@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aܲ-AܸRAܶFAܼjA���A�ƨA�ƨA���A���A���A���A���A���A��#A��AܾwAܶFAܶFAܺ^Aܧ�A܁A�hsA�33A�
=A۩�AڑhA�|�AօA�|�A�hsA�hsA�9XAA��PA��/A�{A�ĜA���A�1'A�C�A�K�A�l�A�XA���A�\)A�|�A��jA�+A��HA�n�A�1A���A��A�l�A� �A��9A��FA�?}A�;dA�  A�S�A�E�A�7LA�7LA��A�r�A��A���A�G�A��A�Q�A���A���A�
=A�ȴA��hA��A�/A��A�|�A��A���A��A�r�A��uA�/A��
A�K�A�I�A�A�A���A��uA��jA�S�A��A��!A�VA���A�/A�x�A��A���A�  A���A��+A��A~$�A|�A{
=AzVAw��Av(�Au"�Ar�jApbAm�mAlA�AjZAh�jAf�AeS�Ac��AaS�A`r�A_oA^M�A^ �A\��AZn�AW�AV�9AVr�AVjAU��AT��AS��APĜAO�hAN��AM+AKS�AJZAIoAHffAHbAG��AF�AFI�AF9XAD�+AC?}AB��ABA�A<~�A:ffA8��A89XA7��A7t�A7VA5�;A4�DA3O�A1�^A0�A/VA.1A,�yA,-A+�#A+�^A+|�A+S�A*�RA*A)x�A'��A&�A&n�A%��A#�A"�A!��A �`A �\A+A�AO�A��AQ�A��A�7Ax�AG�A
=A��A��A"�A1'A�yA�jA$�A�wA�jAbA�A�;A�RA�;A
�HA
ffA
  A�RA�A�!A7LA1'AhsA Ĝ@���@�v�@��-@��@��@�o@��@�%@��@�ƨ@�{@��@� �@�/@���@߮@�S�@�$�@��#@�/@���@��@ە�@��@��@�33@��@�j@Ӆ@�"�@�-@�?}@�V@д9@�1@�C�@�^5@��@ͺ^@�p�@�Ĝ@��@���@��m@���@�t�@�"�@�@��H@ʧ�@�=q@ɺ^@�j@ǥ�@�\)@��H@�^5@�5?@�J@��T@Ł@�&�@��@°!@���@��j@�I�@�o@��^@�z�@���@���@�(�@��@��y@���@�hs@��w@��@���@��^@��@�r�@�r�@�Z@��w@�S�@�@��!@��\@�~�@�M�@��u@�9X@�1'@�9X@�1'@�(�@���@��R@�V@�$�@�@���@���@���@���@�x�@�&�@��@�Q�@�1'@�"�@�v�@���@�x�@�&�@�A�@��m@��
@�ƨ@���@��P@�t�@�\)@�C�@�+@�o@�@�ȴ@��+@�V@�-@���@�?}@��/@��9@�Ĝ@��j@��@��@�+@�
=@�ȴ@�v�@�M�@�@�hs@�%@��@�Ĝ@�b@�1@���@���@���@�l�@��@�=q@���@��^@��@�Z@�9X@�A�@�9X@��@��@�b@�1@�  @� �@�9X@�I�@�I�@�9X@�I�@�(�@��m@���@��F@���@��P@��P@��P@��@�|�@�K�@�
=@�
=@���@��@��!@���@���@���@��\@��+@�^5@�M�@�M�@�-@�J@��^@�V@���@�I�@�1'@� �@�b@��@�dZ@��y@��!@�n�@�M�@���@���@��h@�X@���@���@��@�I�@��@�+@�v�@�=q@�{@��#@��^@��^@���@��7@��7@�x�@�?}@�/@��@�b@��;@��
@�|�@��+@���@�x�@�hs@�%@���@��j@��9@��@�r�@�Z@�(�@��@��w@��@���@�o@�=q@��@���@��@�hs@�G�@�/@��@��G�O�@�"�@|�@p��@hr�@^@XbN@P�@M�@E@@  @8�@3��@1%@+�
@&5?@\)@��@{@  @	��@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BhB\B\B\BJB%BB��B��B�mBɺB��B�\Bp�Be`BZBN�Be`BffB_;B\)BS�BN�BI�BE�BF�BC�B<jB8RB6FB0!B)�B$�B�BuBDBDB1BB��B��B�B�ZB�BB�BB�)B�
B��BÖB�wB�qB�?B�!B��B��B�uBx�B\)BE�B?}B;dB9XB1'B"�BPB�B��B�^B��B{�BdZB\)BP�B=qB�B	7B
��B
�#B
��B
��B
�B
��B
��B
��B
�bB
�%B
w�B
o�B
cTB
^5B
W
B
K�B
>wB
8RB
1'B
#�B
�B
hB
B	�B	�mB	�)B	��B	ȴB	�}B	�FB	�B	��B	��B	��B	�uB	�bB	�+B	|�B	s�B	o�B	n�B	m�B	jB	e`B	_;B	W
B	P�B	L�B	E�B	?}B	;dB	6FB	49B	2-B	0!B	-B	)�B	'�B	!�B	�B	�B	\B��B�B�B�sB�fB�ZB�HB�#B�
B��B��BǮBÖB��B�jB�^B�XB�XB�RB�LB�9B�-B�B�B��B��B��B��B��B��B��B��B�uB�hB�\B�VB�PB�JB�JB�DB�=B�1B�B� B|�By�Bu�Bs�Bq�Bo�Bn�Bk�BjBhsBgmBffBe`BdZBbNB^5B^5BaHBbNBbNBbNBaHBaHB`BB_;B]/B^5B]/B_;B_;B]/B[#BXBYB]/B_;BcTBdZBcTBe`Be`BffBffBhsBhsBiyBk�Bq�Bt�Bx�B|�B{�By�B{�B}�B� B�%B�7B�PB�PB�PB�PB�hB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�XB�RB�XB�qBÖBȴB��B��B��B��B��B�B�fB�yB�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	B	B	B	+B		7B	
=B	
=B	DB	JB	JB	PB	PB	\B	oB	uB	uB	�B	�B	�B	 �B	"�B	'�B	)�B	+B	+B	,B	.B	/B	/B	0!B	1'B	2-B	33B	6FB	8RB	9XB	9XB	;dB	>wB	@�B	A�B	A�B	A�B	E�B	L�B	N�B	N�B	Q�B	VB	W
B	ZB	\)B	^5B	_;B	`BB	gmB	gmB	gmB	jB	n�B	o�B	o�B	r�B	s�B	u�B	z�B	�B	�B	�%B	�%B	�7B	�7B	�=B	�=B	�JB	�\B	�hB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�?B	�LB	�XB	�^B	�^B	�^B	�dB	�jB	�}B	��B	B	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�TB	�ZB	�ZB	�ZB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
bB
�B
$�B
1'B
5?B
:^B
<jB
?}B
C�B
J�B
N�B
P�B
VB
[#B
aHB
gmB
jB
p�B
v�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuBeBeBeBVB2BB��B��B�wB��B��B�cBp�BekBZ#BN�BeeBfpB_?B\1BS�BN�BI�BE�BF�BC�B<qB8YB6PB0(B*B$�B�B{BEBEB6BB��B��B�B�aB�GB�FB�.B�B��BÛB�|B�xB�EB�#B��B��B�uBx�B\-BE�B?B;dB9\B1*B"�BUB�B�B�cB��B{�BdYB\+BP�B=xB�B	<B
��B
�)B
��B
��B
�B
��B
��B
��B
�hB
�,B
w�B
o�B
cZB
^>B
WB
K�B
>�B
8[B
1.B
#�B
�B
rB
B	��B	�yB	�7B	��B	��B	��B	�SB	�B	��B	��B	��B	��B	�sB	�9B	|�B	s�B	o�B	n�B	m�B	j�B	eqB	_MB	WB	P�B	L�B	E�B	?�B	;vB	6XB	4LB	2?B	02B	-B	*B	( B	!�B	�B	�B	pB��B�B�B�B�}B�nB�^B�6B�B�B��B��BëB��B��B�vB�mB�nB�jB�dB�PB�EB�4B�B�B��B��B��B��B��B��B��B��B��B�vB�oB�jB�bB�cB�^B�WB�HB�7B�B}By�Bu�Bs�Bq�Bo�Bn�Bk�Bj�Bh�Bg�BfBeyBdsBbhB^OB^MBabBbhBbjBbgBabBacB`^B_VB]JB^OB]JB_TB_XB]IB[=BX)BY1B]IB_TBcnBdtBcnBe{BezBf�BfBh�Bh�Bi�Bk�Bq�Bt�Bx�B}B{�By�B{�B~B�B�=B�RB�hB�fB�gB�iB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�.B�HB�]B�nB�gB�mB��BêB��B��B��B��B��B��B�3B�zB�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	*B	,B	,B	,B	$B	+B	:B		IB	
OB	
NB	XB	]B	YB	bB	cB	oB	�B	�B	�B	�B	�B	�B	 �B	"�B	(B	*B	+B	+B	,B	.&B	/)B	/,B	02B	17B	2?B	3BB	6VB	8dB	9kB	9hB	;uB	>�B	@�B	A�B	A�B	A�B	E�B	L�B	N�B	N�B	Q�B	VB	WB	Z-B	\:B	^DB	_MB	`PB	g~B	g~B	g{B	j�B	n�B	o�B	o�B	r�B	s�B	u�B	z�B	�"B	�,B	�3B	�2B	�CB	�CB	�KB	�LB	�WB	�iB	�tB	�wB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	�B	�B	�B	�)B	�)B	�)B	�(B	�(B	�3B	�KB	�YB	�fB	�jB	�mB	�mB	�oB	�vB	��B	��B	B	äB	ůB	ǻB	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�-B	�4B	�9B	�:B	�?B	�AB	�FB	�FB	�FB	�OB	�LB	�SB	�bB	�dB	�dB	�eB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�G�O�B
B
oB
�B
$�B
10B
5HB
:fB
<pB
?�B
C�B
J�B
N�B
P�B
VB
[,B
aPB
guB
j�B
p�B
v�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214332016053112143320160531121433  AO  ARCAADJP                                                                    20140721230535    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230535  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230535  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121433  IP                  G�O�G�O�G�O�                