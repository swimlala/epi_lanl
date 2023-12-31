CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-14T14:15:58Z AOML 3.0 creation; 2016-08-07T21:36:33Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150514141558  20160807143633  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               $A   AO  5286_8897_036                   2C  D   APEX                            6531                            072314                          846 @�@w�y��1   @�@x>���@2Y�"��`�c�7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    $A   B   B   @�33@�  A	��A��A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D��D�VfD��fD��fD�	�D�C3D�p D���D��D�,�D���D��3D���D�9�D�s3D���D�  D�33D�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�z�A�A�A@��Ab=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP�\BX��B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C=qC#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C=qC =qC"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D\D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�\D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ\DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt|)Dy��D�!HD�Z�D���D���D�D�G�D�t{D��HD�HD�1HD��D�ǮD��D�>D�w�D��HD�{D�7�D�w�D�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A�"�A�=qA�C�A�G�A�dZA˅AˋDA˛�A��A�C�A�r�A�\)A�jA�x�A�7LA�A�"�A�K�A�1'Aʉ7A�Q�A�7LA��AɸRAɕ�A�C�A�;dA�9XA���AȑhA�VA���A���A���Aǧ�A��AŶFA�S�A�x�A�1'A��^A��A���A��A�z�A���A��9A��A��A�G�A��/A��A�oA���A���A�?}A��A�ȴA��#A��A���A���A�\)A���A�=qA� �A���A��#A��A�=qA�p�A���A�
=A���A�JA�XA��^A�ZA��#A��+A��A���A��9A��`A�^A}t�Ay��Ax��Az(�Ay��AvI�Ar�RAnr�Ahv�Ac�7A^E�AY��AY�AYoAX �AV��AT��AQt�AO"�AK�FAJ�uAJE�AI+AFbAD�AC`BAB$�A@�A>JA<Q�A:�A7��A4(�A2�yA1VA.VA-dZA,ĜA+�TA+oA*{A)VA'�A&��A&�A%�#A$��A$�A#l�A"�DA!`BA ��A =qAG�A�;Ax�AoA��A�A��A�9A�A�uA��A;dA��An�A��AA
=A��A�HA�A��AĜA�A�wA��A�\A�FA��A/A
�A
I�A
(�A
=qA
A�A	�A	��A	&�A�A1'A�AO�A�yAVA��A�+AjAA�A��AA�A�A�-A;dA%A �A 1'@���@���@�hs@�Z@���@���@�r�@�ƨ@�^5@�`B@�V@�C�@�X@�@�@�ƨ@�V@�;d@�?}@�@��@�o@�$�@�G�@ܬ@۝�@�@٩�@�/@ؓu@��H@�x�@��`@�  @�"�@�{@��@�1'@�\)@ΰ!@�^5@�-@��@͙�@�p�@�G�@���@�;d@�hs@У�@�(�@�dZ@Ͳ-@�5?@��@�ȴ@�M�@�E�@�ff@ʟ�@�J@��@�1'@���@Ǿw@ǥ�@�dZ@��y@Ə\@Ɨ�@��T@�?}@��@�/@ģ�@�I�@�1@��;@���@�ƨ@Ý�@�l�@�S�@�o@§�@�v�@��T@�x�@��@���@� �@�\)@�;d@�
=@�ff@���@���@�@���@���@�O�@��/@���@� �@�+@�^5@�$�@�-@��+@�n�@���@���@�O�@�7L@��h@�x�@�/@��u@� �@��@�  @��@�t�@���@�S�@���@�@���@��@�Q�@��@���@���@�t�@�S�@��@��\@�M�@���@�hs@�&�@��@�A�@�ƨ@���@�t�@�33@��+@�V@�M�@�E�@���@���@��9@�z�@�I�@�b@��@���@�|�@�C�@�
=@��\@�5?@�@��@���@��^@���@��@�X@��@���@�bN@�1@��m@���@���@�|�@�K�@��!@�^5@�$�@�@��@���@�p�@���@���@�p�@��@���@��/@���@��m@�S�@�
=@���@��T@���@��-@�O�@���@���@�I�@�(�@��@��@�t�@�K�@�K�@�+@���@�-@��@�@�`B@�V@��@��@�j@�1'@���@��;@���@�"�@��y@��H@�ȴ@�M�@�@���@�G�@��`@�z�@�A�@��m@���@���@���@�O�@�/@�V@���@���@��@��D@�A�@��
@���@�t�@�\)@�+@�@��H@�ȴ@���@�v�@���@�x�@�&�@��@��`@���@�z�@�Z@���@�ƨ@���@��;@��
@��w@�"�@���@��!@��\@�V@�=q@�J@��!@��-@���@x�9@j-@d1@["�@T��@M`B@E��@?�@8�9@1��@+C�@&ff@!��@��@+@(�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A�"�A�=qA�C�A�G�A�dZA˅AˋDA˛�A��A�C�A�r�A�\)A�jA�x�A�7LA�A�"�A�K�A�1'Aʉ7A�Q�A�7LA��AɸRAɕ�A�C�A�;dA�9XA���AȑhA�VA���A���A���Aǧ�A��AŶFA�S�A�x�A�1'A��^A��A���A��A�z�A���A��9A��A��A�G�A��/A��A�oA���A���A�?}A��A�ȴA��#A��A���A���A�\)A���A�=qA� �A���A��#A��A�=qA�p�A���A�
=A���A�JA�XA��^A�ZA��#A��+A��A���A��9A��`A�^A}t�Ay��Ax��Az(�Ay��AvI�Ar�RAnr�Ahv�Ac�7A^E�AY��AY�AYoAX �AV��AT��AQt�AO"�AK�FAJ�uAJE�AI+AFbAD�AC`BAB$�A@�A>JA<Q�A:�A7��A4(�A2�yA1VA.VA-dZA,ĜA+�TA+oA*{A)VA'�A&��A&�A%�#A$��A$�A#l�A"�DA!`BA ��A =qAG�A�;Ax�AoA��A�A��A�9A�A�uA��A;dA��An�A��AA
=A��A�HA�A��AĜA�A�wA��A�\A�FA��A/A
�A
I�A
(�A
=qA
A�A	�A	��A	&�A�A1'A�AO�A�yAVA��A�+AjAA�A��AA�A�A�-A;dA%A �A 1'@���@���@�hs@�Z@���@���@�r�@�ƨ@�^5@�`B@�V@�C�@�X@�@�@�ƨ@�V@�;d@�?}@�@��@�o@�$�@�G�@ܬ@۝�@�@٩�@�/@ؓu@��H@�x�@��`@�  @�"�@�{@��@�1'@�\)@ΰ!@�^5@�-@��@͙�@�p�@�G�@���@�;d@�hs@У�@�(�@�dZ@Ͳ-@�5?@��@�ȴ@�M�@�E�@�ff@ʟ�@�J@��@�1'@���@Ǿw@ǥ�@�dZ@��y@Ə\@Ɨ�@��T@�?}@��@�/@ģ�@�I�@�1@��;@���@�ƨ@Ý�@�l�@�S�@�o@§�@�v�@��T@�x�@��@���@� �@�\)@�;d@�
=@�ff@���@���@�@���@���@�O�@��/@���@� �@�+@�^5@�$�@�-@��+@�n�@���@���@�O�@�7L@��h@�x�@�/@��u@� �@��@�  @��@�t�@���@�S�@���@�@���@��@�Q�@��@���@���@�t�@�S�@��@��\@�M�@���@�hs@�&�@��@�A�@�ƨ@���@�t�@�33@��+@�V@�M�@�E�@���@���@��9@�z�@�I�@�b@��@���@�|�@�C�@�
=@��\@�5?@�@��@���@��^@���@��@�X@��@���@�bN@�1@��m@���@���@�|�@�K�@��!@�^5@�$�@�@��@���@�p�@���@���@�p�@��@���@��/@���@��m@�S�@�
=@���@��T@���@��-@�O�@���@���@�I�@�(�@��@��@�t�@�K�@�K�@�+@���@�-@��@�@�`B@�V@��@��@�j@�1'@���@��;@���@�"�@��y@��H@�ȴ@�M�@�@���@�G�@��`@�z�@�A�@��m@���@���@���@�O�@�/@�V@���@���@��@��D@�A�@��
@���@�t�@�\)@�+@�@��H@�ȴ@���@�v�@���@�x�@�&�@��@��`@���@�z�@�Z@���@�ƨ@���@��;@��
@��w@�"�@���@��!@��\@�V@�=qG�O�@��!@��-@���@x�9@j-@d1@["�@T��@M`B@E��@?�@8�9@1��@+C�@&ff@!��@��@+@(�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�%B	�%B	�B	�B	�B	�%B	�+B	�+B	�1B	�+B	�1B	�1B	�1B	�7B	�bB	�hB	�oB	��B	��B	��B	��B	�jB	�BB
DB
�B
%�B
.B
33B
_;B
�BBJ�B�B�XB��B�#B�ZB�B��BPB{B�B'�BH�BO�BQ�BP�BO�BN�BdZBv�Bn�BVBB�B�sB��B�'B�uBe`B]/BYBXBZBW
BVBT�BYB`BBw�B�%B�Bs�Bo�B\)BP�B@�B1'B8RB6FB&�BuB
�B
��B
�LB
��B
�hB
�7B
p�B
W
B
33B
6FB
7LB
$�B
�B	��B	�`B	�B	�yB	�HB	�B	��B	��B	��B	��B	�^B	��B	|�B	S�B	49B	�B	B	B��B��B�B�fB�B��BƨBB�}B�XB�-B�B�B��B��B�XB�qB�^B�?B�wB�jB�}B�FB�?B�9B�LB�LB�RB�XB�jB�dB�dB�dB�dB�^B�RB�RB�RB�FB�?B�3B�LB�^B�jB��B�jB�-B��B��B�B�B��B�B�RB�FBBɺBɺBɺBȴBȴBɺB��B��B��B��B�B�B��BǮB��B�BB�B��B	B	B��B��B	B��B��B��B��B��B��B��B	B	B	B	B	B	B	B	B	B��B�B�B�B�B�B��B��B	B��B��B�B�HB�5B�/B�/B�#B�B�B�B�B�B�5B�;B�;B�BB�NB�HB�NB�NB�HB�yB�B�B�B�B��B��B��B��B	  B	B	+B		7B	
=B	PB	hB	�B	$�B	8RB	=qB	C�B	C�B	@�B	C�B	F�B	>wB	?}B	A�B	E�B	J�B	J�B	I�B	P�B	T�B	W
B	YB	]/B	dZB	gmB	hsB	l�B	p�B	s�B	w�B	x�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�JB	�VB	�VB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�!B	�B	�!B	�!B	�3B	�?B	�FB	�LB	�LB	�FB	�^B	�dB	�jB	�qB	�}B	�wB	�qB	�jB	�dB	�dB	�^B	�^B	�^B	�jB	�jB	�qB	�}B	��B	B	B	ĜB	ǮB	ǮB	ȴB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�5B	�;B	�BB	�HB	�TB	�ZB	�mB	�sB	�sB	�sB	�yB	�yB	�sB	�mB	�sB	�B	�yB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
%B
+B
+B
+B
	7B
	7B
	7B
	7B
	7B
JB
DB
hB
�B
&�B
&�B
+B
2-B
7LB
=qB
D�B
I�B
P�B
S�B
[#B
`BB
cTB
gmB
l�B
n�B
s�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�B	�B	�&B	�&B	�B	�!B	�B	�&B	�-B	�-B	�3B	�-B	�3B	�4B	�4B	�:B	�dB	�kB	�qB	��B	��B	��B	��B	�iB	�AB
BB
�B
%�B
.B
32B
_7B
�:BJ�B�B�LB˻B�B�LB�B��BDBoB�B'�BH�BO�BQ�BP�BO�BN�BdPBv�Bn�BU�BB�B�iB��B�B�iBeTB]#BY
BXBZBV�BU�BT�BY	B`5Bw�B�B�Bs�Bo�B\BP�B@wB1B8GB6;B&�BkB
�B
��B
�CB
��B
�]B
�,B
p�B
WB
3-B
6>B
7DB
$�B
�B	��B	�\B	�B	�tB	�CB	��B	��B	��B	��B	��B	�[B	��B	|�B	S�B	4;B	�B	"B		B��B��B�B�iB�B��BƫBB��B�[B�2B�B�B��B��B�ZB�uB�cB�DB�{B�mB��B�JB�CB�>B�NB�OB�TB�[B�kB�fB�hB�fB�hB�aB�VB�VB�UB�KB�DB�6B�PB�_B�lB��B�mB�/B��B��B�B�B��B�B�SB�JBBɼBɽBɾBȶBȷBɺB��B��B��B��B�B�B��BǭB��B�CB�B��B	B	B��B��B	B��B��B��B��B��B��B��B	B	B	B	B	B	B	B	B	
B��B�B�B�B�B�B��B��B	B��B��B�B�IB�6B�0B�0B�#B�B�B�B�B�B�4B�<B�9B�BB�LB�GB�NB�NB�FB�yB�B�B�B�B��B��B��B��B��B	B	)B		3B	
:B	LB	dB	�B	$�B	8OB	=mB	C�B	C�B	@}B	C�B	F�B	>qB	?wB	A�B	E�B	J�B	J�B	I�B	P�B	T�B	WB	YB	]*B	dUB	ggB	hnB	l�B	p�B	s�B	w�B	x�B	z�B	|�B	~�B	��B	�B	�B	�B	�B	�B	�0B	�7B	�CB	�LB	�NB	�SB	�]B	�tB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�
B	�B	�B	�B	�B	�B	�*B	�5B	�<B	�AB	�BB	�=B	�VB	�]B	�aB	�fB	�rB	�mB	�gB	�aB	�[B	�^B	�VB	�TB	�UB	�`B	�aB	�gB	�rB	��B	B	B	ēB	ǦB	ǧB	ȪB	ǥB	ȪB	ȪB	ɰB	ʸB	˽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	� B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�+B	�*B	�/B	�7B	�>B	�GB	�NB	�bB	�iB	�hB	�gB	�mB	�oB	�gB	�cB	�gB	�tB	�pB	�iB	�kB	�jB	�kB	�sB	�rB	�tB	�zB	�uB	�vB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B
 �B
 �B
 �B
 �B
B
B
 B
 �B
 �B
 �B
 �B
 �B	��B
 �B
 �B
B
B
B
B
B
 B
 B
 B
	*B
	+B
	+B
	,B
	*G�O�B
8B
ZB
�B
&�B
&�B
*�B
2 B
7@B
=eB
D�B
I�B
P�B
S�B
[B
`3B
cFB
gbB
l|B
n�B
s�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436332016080714363320160807143633  AO  ARCAADJP                                                                    20150514141558    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150514141558  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150514141558  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143633  IP                  G�O�G�O�G�O�                