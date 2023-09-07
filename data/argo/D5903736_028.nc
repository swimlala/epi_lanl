CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:20Z AOML 3.0 creation; 2016-05-31T19:14:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230520  20160531121429  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_028                   2C  D   APEX                            5368                            041511                          846 @�y-W?�1   @�y-�M� @4����l��d��7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dys3D�	�D�P D�y�D���D�fD�FfD�|�D���D�3D�<�D�s3D���D���D�FfDړ3D�ٚD�fD�C3D��D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ff@�ffA ��A?33A_33A33A���A���A���A���Aϙ�Aߙ�AB 33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B�L�B��3B��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C\�C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D�gDvgD��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D%3D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dyp D� D�NfD�x D��3D��D�D�D�{3D��3D��D�;3D�q�D��3D��3D�D�Dڑ�D�� D��D�A�D�3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A��
A��#A��#A��#A��#A��;A��;A��;A��HA��HA��TA��`A��`A��`A��`A��mA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A��mA�ĜA�hsA��HA�Aǟ�A�-A�v�A�A��yA��/A��A��A�5?A��+A�1'A�C�A�5?A��A�&�A�jA��mA���A��TA�ȴA���A��A�$�A���A�A�t�A�7LA���A��A�n�A���A��A��A�t�A��^A���A��A�`BA�1'A�~�A���A��/A��A��yA�dZA��yA���A�1'A�A�`BA�n�A�&�A�JA�l�A��/A��
A���A�l�A�oA��+A���A��A��/A}�mAz  Avr�Aux�AsC�ArjAqK�An��Am�Am?}Ak��Ai�^Ag�#Af��AfJAe��Ad=qAc��Aa�wA_�wA^jA]x�A\�yA[��A[AW+AVn�AV=qAU��AUƨAT�AT5?AS+AR�\AR(�AQ\)AP�AN��AMS�AK�wAJ  AIAI�PAI+AH�\AG��AF�RAFJADAA�mA@ĜA?S�A=A<~�A;�A:�`A8��A7oA5�A5A2ffA0��A0  A.~�A-;dA,�uA+A)�TA(-A'?}A&E�A$��A$�DA$A�A#��A"�/A"�A!�TA ��A $�A�A"�A�+A�wA��At�AjA�uA�+A��A�hA  A�jAM�A�A��A��AXA~�At�A	�A	;dA	%Ar�A��A�HA��A��A�DA�A?}A 9X@�;d@�O�@��R@�?}@��
@��@�V@�/@���@�?}@��@�b@�F@�P@�@�{@��-@�G�@���@���@�ƨ@睲@�dZ@���@�ff@�X@�P@�ff@�X@�I�@�l�@�^5@��`@�I�@ۅ@��@ش9@�E�@�;d@Ѻ^@�r�@�  @�+@Ο�@�x�@�1@���@��@Ǯ@���@�X@��/@�1@Õ�@�ȴ@���@�&�@��/@��D@��@��@���@�^5@�J@��T@��h@�hs@�/@�1'@�C�@���@��@�@�&�@��@�A�@��@���@�~�@���@��@�G�@��@�V@��D@��m@��@��@���@�ff@�@��9@�r�@�Q�@�9X@��
@��H@�V@��#@�G�@���@�b@��P@�|�@�K�@���@�n�@�ff@�M�@��@���@��@�x�@�`B@�O�@�?}@��@��`@�j@��
@��P@�\)@�+@�@���@�$�@�@��#@�@���@��h@��/@�Q�@��m@���@�;d@�@���@���@���@�~�@�5?@���@��@��-@��@��@��@���@�Z@�  @���@��@���@��\@�n�@�=q@��@��-@���@�7L@���@��@�1'@��w@���@�|�@�l�@�
=@��!@�M�@�J@��@���@��^@��h@�7L@��9@��@���@�z�@�9X@��@��;@��@�
=@���@��\@�V@�-@�$�@�{@���@�p�@��/@�Ĝ@��j@�Ĝ@��@��u@�  @��F@���@��@�t�@�33@��@��!@�v�@�V@�J@��@���@��7@�O�@��@�Ĝ@���@�I�@� �@�b@��m@��w@�;d@��R@�V@�$�@���@���@���@�x�@�G�@��`@��@���@�z�@�A�@�b@��F@�S�@�@��@��y@��H@���@���@�v�@�E�@�-@�J@��T@��@�&�@��@��@��@��j@��u@��@�Z@�I�@�1'@�(�@��@��m@��;@�E�@�Ĝ@vff@l�/@a��@X�u@Q��@J��@A�@9�#@4�@/�;@+@&��@!�@�@M�@�h@��@�j@	�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A��
A��#A��#A��#A��#A��;A��;A��;A��HA��HA��TA��`A��`A��`A��`A��mA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A��mA�ĜA�hsA��HA�Aǟ�A�-A�v�A�A��yA��/A��A��A�5?A��+A�1'A�C�A�5?A��A�&�A�jA��mA���A��TA�ȴA���A��A�$�A���A�A�t�A�7LA���A��A�n�A���A��A��A�t�A��^A���A��A�`BA�1'A�~�A���A��/A��A��yA�dZA��yA���A�1'A�A�`BA�n�A�&�A�JA�l�A��/A��
A���A�l�A�oA��+A���A��A��/A}�mAz  Avr�Aux�AsC�ArjAqK�An��Am�Am?}Ak��Ai�^Ag�#Af��AfJAe��Ad=qAc��Aa�wA_�wA^jA]x�A\�yA[��A[AW+AVn�AV=qAU��AUƨAT�AT5?AS+AR�\AR(�AQ\)AP�AN��AMS�AK�wAJ  AIAI�PAI+AH�\AG��AF�RAFJADAA�mA@ĜA?S�A=A<~�A;�A:�`A8��A7oA5�A5A2ffA0��A0  A.~�A-;dA,�uA+A)�TA(-A'?}A&E�A$��A$�DA$A�A#��A"�/A"�A!�TA ��A $�A�A"�A�+A�wA��At�AjA�uA�+A��A�hA  A�jAM�A�A��A��AXA~�At�A	�A	;dA	%Ar�A��A�HA��A��A�DA�A?}A 9X@�;d@�O�@��R@�?}@��
@��@�V@�/@���@�?}@��@�b@�F@�P@�@�{@��-@�G�@���@���@�ƨ@睲@�dZ@���@�ff@�X@�P@�ff@�X@�I�@�l�@�^5@��`@�I�@ۅ@��@ش9@�E�@�;d@Ѻ^@�r�@�  @�+@Ο�@�x�@�1@���@��@Ǯ@���@�X@��/@�1@Õ�@�ȴ@���@�&�@��/@��D@��@��@���@�^5@�J@��T@��h@�hs@�/@�1'@�C�@���@��@�@�&�@��@�A�@��@���@�~�@���@��@�G�@��@�V@��D@��m@��@��@���@�ff@�@��9@�r�@�Q�@�9X@��
@��H@�V@��#@�G�@���@�b@��P@�|�@�K�@���@�n�@�ff@�M�@��@���@��@�x�@�`B@�O�@�?}@��@��`@�j@��
@��P@�\)@�+@�@���@�$�@�@��#@�@���@��h@��/@�Q�@��m@���@�;d@�@���@���@���@�~�@�5?@���@��@��-@��@��@��@���@�Z@�  @���@��@���@��\@�n�@�=q@��@��-@���@�7L@���@��@�1'@��w@���@�|�@�l�@�
=@��!@�M�@�J@��@���@��^@��h@�7L@��9@��@���@�z�@�9X@��@��;@��@�
=@���@��\@�V@�-@�$�@�{@���@�p�@��/@�Ĝ@��j@�Ĝ@��@��u@�  @��F@���@��@�t�@�33@��@��!@�v�@�V@�J@��@���@��7@�O�@��@�Ĝ@���@�I�@� �@�b@��m@��w@�;d@��R@�V@�$�@���@���@���@�x�@�G�@��`@��@���@�z�@�A�@�b@��F@�S�@�@��@��y@��H@���@���@�v�@�E�@�-@�J@��T@��@�&�@��@��@��@��j@��u@��@�Z@�I�@�1'@�(�@��@��m@��;@�E�@�Ĝ@vff@l�/@a��@X�u@Q��@J��@A�@9�#@4�@/�;@+@&��@!�@�@M�@�h@��@�j@	�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBŢBƨBƨBƨBƨBŢBŢBŢBŢBŢBƨBƨBƨBƨBŢBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBȴB��B��B��B��B��B�/B�B�BJB �B0!B0!B)�B!�BbB	7B	7B+BB  B��B��B��B��B��B��B��B��B  B��B��B��B�B�B�B�mB�BƨB�}B�9B��B��B�\B�By�Bp�BffBG�B)�BB��B�B�HB�B��B��B��B�RB�!B��Bl�BZBL�B0!B%B
�TB
ÖB
��B
n�B
P�B
9XB
#�B
bB
  B	�B	�`B	�#B	��B	��B	ŢB	B	��B	�qB	�?B	�'B	�B	��B	��B	��B	��B	�hB	�%B	~�B	x�B	t�B	l�B	e`B	T�B	P�B	P�B	O�B	N�B	L�B	I�B	E�B	B�B	?}B	;dB	5?B	/B	'�B	 �B	�B	�B	�B	�B	oB	VB	
=B	B��B��B�B�mB�NB�5B�)B�B��BȴBŢBB�qB�dB�RB�9B�'B�B��B��B��B��B��B��B��B��B��B��B�uB�oB�\B�PB�JB�DB�=B�1B�B�B}�By�Bu�Br�Bp�Bn�Bm�Bm�Bl�Bm�Bm�Bm�Bm�Bk�Bl�Bl�Bo�Bo�Bp�Bl�BffBgmBgmBgmBaHBaHB`BB`BBcTBffBjBjBiyBjBk�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bm�Bm�Bl�Bk�Bl�Bm�Bn�Bp�Bs�Bv�Bx�Bz�Bz�B}�B~�B� B�B�B�B�B�B�7B�\B�oB�{B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�?B�jBBŢBǮBɺB��B��B��B��B��B��B�B�
B�)B�NB�fB�yB�B�B��B��B	  B	B	B	%B	
=B	DB	PB	JB	VB	hB	oB	�B	�B	�B	�B	"�B	"�B	#�B	#�B	%�B	)�B	.B	2-B	2-B	49B	6FB	9XB	?}B	@�B	C�B	E�B	E�B	E�B	H�B	I�B	J�B	J�B	J�B	K�B	K�B	L�B	M�B	O�B	S�B	VB	XB	ZB	ZB	\)B	`BB	cTB	hsB	hsB	iyB	k�B	n�B	o�B	p�B	r�B	t�B	v�B	w�B	x�B	y�B	z�B	|�B	~�B	~�B	� B	�B	�B	�B	�%B	�7B	�DB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�'B	�-B	�-B	�9B	�?B	�FB	�LB	�XB	�^B	�^B	�^B	�dB	�dB	�jB	�wB	��B	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�;B	�BB	�HB	�NB	�NB	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
\B
�B
�B
)�B
/B
7LB
=qB
F�B
L�B
R�B
W
B
\)B
`BB
cTB
e`B
iyB
l�B
o�B
t�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BŤBŤBŧBŧBŤBŧBŤBŧBšBŧBŤBƭBƫBƭBƯBŤBŤBŤBŧBŧBƫBƫBƭBƭBŨBƭBƭBƫBƭBƭBƭBƭBƭBƭBƭBƭBƭBȺB��B��B��B��B��B�7B�B�BNB �B0(B0*B*B!�BiB	>B	?B2BB B��B��B��B��B��B��B��B�B B��B��B��B�B�B�B�vB�BƫB��B�BB��B��B�cB�&By�Bp�BflBG�B*BB��B�B�LB�B��B��B��B�YB�&B��Bl�BZ%BL�B0'B,B
�ZB
ßB
��B
n�B
P�B
9bB
#�B
pB
 B	�B	�lB	�1B	�B	��B	ŰB	B	��B	��B	�OB	�:B	�B	�B	��B	��B	��B	�xB	�6B	B	x�B	t�B	l�B	eqB	UB	P�B	P�B	O�B	N�B	L�B	I�B	E�B	B�B	?�B	;wB	5SB	/0B	(B	 �B	�B	�B	�B	�B	�B	jB	
TB	6B�B��B��B�B�gB�OB�CB�*B��B��BŻB¨B��B�}B�hB�QB�@B�.B�B�B��B��B��B��B��B��B��B��B��B��B�xB�jB�eB�_B�VB�NB�:B�!B~By�Bu�Br�Bp�Bn�Bm�Bm�Bl�Bm�Bm�Bm�Bm�Bk�Bl�Bl�Bo�Bo�Bp�Bl�Bf�Bg�Bg�Bg�BaeBafB`aB`_BcsBf�Bj�Bj�Bi�Bj�Bk�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bm�Bm�Bl�Bk�Bl�Bm�Bn�Bp�Bs�Bv�Bx�Bz�Bz�B~BB�B�(B�(B�-B�:B�:B�TB�xB��B��B��B��B��B��B��B�B�B�B�#B�/B�9B�GB�NB�YB��B§BŹB��B��B��B��B��B� B�
B�B�B� B�@B�fB�}B�B�B�B��B��B	 B	$B	0B	9B	
RB	XB	fB	^B	jB	~B	�B	�B	�B	�B	�B	"�B	"�B	#�B	#�B	%�B	*B	.)B	2@B	2AB	4LB	6XB	9jB	?�B	@�B	C�B	E�B	E�B	E�B	H�B	I�B	J�B	J�B	J�B	K�B	K�B	L�B	M�B	O�B	TB	VB	X"B	Z0B	Z1B	\<B	`UB	ceB	h�B	h�B	i�B	k�B	n�B	o�B	p�B	r�B	t�B	v�B	w�B	x�B	y�B	z�B	} B	B	B	�B	�)B	�0B	�)B	�2B	�GB	�TB	�fB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�6B	�<B	�?B	�HB	�OB	�SB	�\B	�hB	�lB	�mB	�mB	�vB	�vB	�zB	��B	��B	��B	æB	ĪB	ĬB	īB	ĬB	ŰB	ǾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�$B	�*B	�1B	�7B	�?B	�HB	�PB	�UB	�ZB	�[B	�nB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B
 B
"B
"B
B
 B
!B
*B
4B
hB
�B
�B
*B
/(B
7YB
=~B
F�B
L�B
R�B
WB
\2B
`LB
c_B
emB
i�B
l�B
o�B
t�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214292016053112142920160531121429  AO  ARCAADJP                                                                    20140721230520    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230520  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230520  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121429  IP                  G�O�G�O�G�O�                