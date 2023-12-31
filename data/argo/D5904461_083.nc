CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-13T20:18:11Z AOML 3.0 creation; 2016-08-07T21:36:40Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151113201811  20160807143641  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               SA   AO  5286_8897_083                   2C  D   APEX                            6531                            072314                          846 @�~p���1   @�~q���L@3V��c^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    SA   B   B   @���@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�33A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` DyY�D�  D�@ D�y�D�� D�	�D�<�D�p D��3D�fD�33D���D��3D���D�FfD�vfDਗ਼D�	�D�C3D�l�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@�z�A=qA"=qAB=qA`��A��A��A��A��A��A�Q�A��A��B ��B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf=qCh=qCj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dth�Dyb�D�${D�D{D�~D��{D�D�AHD�t{D�׮D�
�D�7�D��D�ǮD��D�J�D�z�D�D�D�G�D�qHD�Ǯ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�z�Aׇ+Aׇ+AׅA׃AׅAׅAׅAׇ+A׉7A׉7A׉7Aׇ+A׉7A׉7A׋DA׍PA׍PA׍PA׍PAבhAבhA׏\A׏\AבhAדuAח�Aי�Aכ�Aכ�Aם�Aכ�Aׇ+AׁA�bNAּjAոRA�`BA��A�~�A˶FA�/A�~�A���A���AċDA�A�7LA��TA�
=A�z�A��7A�9XA��+A���A�x�A���A��;A��A���A�{A��\A�5?A�{A�=qA���A�ffA��A�`BA�ƨA��A�t�A�A�z�A�p�A��+A�A�A�
=A�ĜA�x�A��hA��RA���A��A���A� �A���A�JA���A�VA�x�A��A�ffA���A�Q�A��A�G�A�z�A�bA���A�hsA��RA�(�A��+A�VA��9A���A�O�A~�A|VAzn�AydZAv��Ap�RAo/An�Al$�AfI�AdĜAa&�A^~�A]t�A[��AY\)AVZAUC�ARffAP �AO��AO�hAOVAL �AG%AFbNAD5?AB��ABJAA�PA?�PA>(�A=S�A;t�A:$�A9G�A7��A5�A5�A4�DA3�wA2 �A/hsA-A-�A,�A+
=A*bNA)�PA(��A'�#A&VA$n�A"��A"n�A!�A ffA|�A;dA�mAr�AI�A�A�jA�HAoAAA^5A�PA�A��A��AȴA{A�^A
=A^5A|�A�RA`BA\)A^5A�wA��A�TAx�A�A��Az�Az�Av�AffA=qA�AjA�wA7LA
�RA
=qA
$�A	�7A	dZA	O�A��A{A&�A�wA5?AM�A �u@��w@��!@���@�n�@�-@�z�@��@�dZ@���@��h@�D@�@�v�@�-@�/@��
@�ff@��m@�X@�j@���@�@�
=@��@�Z@�@�1'@�|�@���@�(�@֟�@Ձ@ղ-@���@�Z@Ұ!@��#@�@�hs@�  @�@���@̃@��@���@�{@ȃ@��@�=q@�X@��T@ƸR@ŉ7@��/@�(�@���@�V@�ƨ@�b@�K�@�
=@�hs@�x�@���@���@���@�%@�x�@��P@��@�V@�33@�5?@��#@���@��-@��@���@���@�o@��@�33@�C�@��@�Q�@�j@�z�@�A�@���@��@�"�@���@� �@��@�S�@��
@�(�@�b@���@���@���@��j@���@���@�$�@�@�@�o@��@��-@��\@���@���@��#@��
@�-@�X@���@��y@��@�$�@�hs@��@�Ĝ@�K�@��@��7@���@�E�@�~�@��+@��@�b@���@�dZ@���@� �@��;@�A�@��9@��D@�?}@�z�@��
@�@�@��@��R@���@�;d@�?}@�-@�@���@�v�@�5?@�{@���@��^@��@��@�&�@�A�@���@�K�@�
=@��+@�E�@���@��-@�hs@�/@�&�@�X@�G�@�/@�7L@��9@�Z@� �@�  @�Q�@��@�bN@�1'@�(�@��@�l�@�S�@�S�@�C�@�33@���@�5?@���@��@�(�@�ƨ@�;d@���@��@�7L@���@���@�$�@���@���@��@�X@�/@��9@���@���@�z�@�Z@�Q�@�r�@�Ĝ@���@��@��7@�O�@��@���@��@� �@��@��w@�\)@���@���@�ȴ@�ȴ@�ȴ@���@���@�~�@���@�x�@�O�@�G�@�O�@�G�@��/@�Q�@���@���@���@�^5@���@��T@��#@���@��7@��@�Ĝ@��9@��@���@�z�@�Z@�Q�@� �@�dZ@�"�@�"�@��y@�=q@�/@y�@mO�@d�/@[��@Rn�@J�\@B�!@<�@7|�@2-@,�@'K�@!�#@�!@��@^5@�@��@��@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�v�A�z�Aׇ+Aׇ+AׅA׃AׅAׅAׅAׇ+A׉7A׉7A׉7Aׇ+A׉7A׉7A׋DA׍PA׍PA׍PA׍PAבhAבhA׏\A׏\AבhAדuAח�Aי�Aכ�Aכ�Aם�Aכ�Aׇ+AׁA�bNAּjAոRA�`BA��A�~�A˶FA�/A�~�A���A���AċDA�A�7LA��TA�
=A�z�A��7A�9XA��+A���A�x�A���A��;A��A���A�{A��\A�5?A�{A�=qA���A�ffA��A�`BA�ƨA��A�t�A�A�z�A�p�A��+A�A�A�
=A�ĜA�x�A��hA��RA���A��A���A� �A���A�JA���A�VA�x�A��A�ffA���A�Q�A��A�G�A�z�A�bA���A�hsA��RA�(�A��+A�VA��9A���A�O�A~�A|VAzn�AydZAv��Ap�RAo/An�Al$�AfI�AdĜAa&�A^~�A]t�A[��AY\)AVZAUC�ARffAP �AO��AO�hAOVAL �AG%AFbNAD5?AB��ABJAA�PA?�PA>(�A=S�A;t�A:$�A9G�A7��A5�A5�A4�DA3�wA2 �A/hsA-A-�A,�A+
=A*bNA)�PA(��A'�#A&VA$n�A"��A"n�A!�A ffA|�A;dA�mAr�AI�A�A�jA�HAoAAA^5A�PA�A��A��AȴA{A�^A
=A^5A|�A�RA`BA\)A^5A�wA��A�TAx�A�A��Az�Az�Av�AffA=qA�AjA�wA7LA
�RA
=qA
$�A	�7A	dZA	O�A��A{A&�A�wA5?AM�A �u@��w@��!@���@�n�@�-@�z�@��@�dZ@���@��h@�D@�@�v�@�-@�/@��
@�ff@��m@�X@�j@���@�@�
=@��@�Z@�@�1'@�|�@���@�(�@֟�@Ձ@ղ-@���@�Z@Ұ!@��#@�@�hs@�  @�@���@̃@��@���@�{@ȃ@��@�=q@�X@��T@ƸR@ŉ7@��/@�(�@���@�V@�ƨ@�b@�K�@�
=@�hs@�x�@���@���@���@�%@�x�@��P@��@�V@�33@�5?@��#@���@��-@��@���@���@�o@��@�33@�C�@��@�Q�@�j@�z�@�A�@���@��@�"�@���@� �@��@�S�@��
@�(�@�b@���@���@���@��j@���@���@�$�@�@�@�o@��@��-@��\@���@���@��#@��
@�-@�X@���@��y@��@�$�@�hs@��@�Ĝ@�K�@��@��7@���@�E�@�~�@��+@��@�b@���@�dZ@���@� �@��;@�A�@��9@��D@�?}@�z�@��
@�@�@��@��R@���@�;d@�?}@�-@�@���@�v�@�5?@�{@���@��^@��@��@�&�@�A�@���@�K�@�
=@��+@�E�@���@��-@�hs@�/@�&�@�X@�G�@�/@�7L@��9@�Z@� �@�  @�Q�@��@�bN@�1'@�(�@��@�l�@�S�@�S�@�C�@�33@���@�5?@���@��@�(�@�ƨ@�;d@���@��@�7L@���@���@�$�@���@���@��@�X@�/@��9@���@���@�z�@�Z@�Q�@�r�@�Ĝ@���@��@��7@�O�@��@���@��@� �@��@��w@�\)@���@���@�ȴ@�ȴ@�ȴ@���@���@�~�@���@�x�@�O�@�G�@�O�@�G�@��/@�Q�@���@���@���@�^5@���@��T@��#@���@��7@��@�Ĝ@��9@��@���@�z�@�Z@�Q�@� �@�dZ@�"�@�"�@��yG�O�@�/@y�@mO�@d�/@[��@Rn�@J�\@B�!@<�@7|�@2-@,�@'K�@!�#@�!@��@^5@�@��@��@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�BD�BD�BD�BD�BD�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BE�BF�BG�BK�BL�BP�Bm�B�DB��BPB$�B%�B'�BW
B�Bu�B{�B�B�bB�1B�B�B{�B��B��B��B��B�;B�yB�B�yB�B�B�sB�;B��B�dB�VB#�B��B�B�B�B��B��B��B�{B�VBq�B`BBQ�BE�B:^B2-B�B
�BbB�B%B
��B
��B
�fB
�ZB
�;B
��B
�!B
s�B
I�B
R�B
_;B
bNB
dZB
_;B
T�B
I�B
?}B
8RB
-B
!�B
{B
B	��B	�B	��B	�'B	��B	��B	�B	gmB	_;B	Q�B	>wB	6FB	.B	�B	JB	B	B��B�B�B�yB�
B��B��BÖB�qB�qB�dB�3B�B��B��B��B�uB�hB�oB�oB�uB�uB�bB�\B�PB�JB�=B�1B�+B�B�B�B{�By�Bw�Bs�Bq�Bv�Bx�By�Bx�B|�B�B�B�B}�BjB^5Bm�Br�Bp�Bo�Bn�BjBhsBhsBhsBl�Bt�B�hB�!BÖBƨBǮBĜBĜB��B��B�TB�ZB�mB�sB�yB�yB�B�B��B	  B	B	1B	bB	�B	�B	�B	#�B	)�B	,B	(�B	"�B	�B	DB	B��B��B	DB	DB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�ZB�B�B��B��B��B��B��B��B��B��B	B	B	B	B	1B	
=B	uB	�B	�B	#�B	'�B	-B	.B	6FB	=qB	I�B	J�B	K�B	L�B	C�B	:^B	C�B	D�B	A�B	8RB	33B	5?B	9XB	D�B	K�B	W
B	^5B	VB	K�B	M�B	K�B	P�B	T�B	T�B	W
B	W
B	R�B	R�B	W
B	ZB	^5B	`BB	gmB	q�B	u�B	y�B	{�B	{�B	{�B	y�B	u�B	u�B	w�B	|�B	�B	�7B	�PB	�JB	�VB	�=B	�%B	}�B	u�B	m�B	m�B	o�B	u�B	�B	�B	�7B	�DB	�=B	�%B	~�B	x�B	u�B	u�B	o�B	m�B	n�B	n�B	l�B	k�B	ffB	cTB	dZB	ffB	l�B	o�B	p�B	v�B	�B	�bB	��B	��B	��B	��B	�B	�B	�B	�?B	�?B	�?B	�RB	�9B	�!B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�3B	�3B	�3B	�9B	�FB	�?B	�FB	�RB	�RB	�jB	�jB	�jB	�jB	�jB	�jB	�wB	��B	B	ÖB	ƨB	ŢB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�/B	�HB	�HB	�;B	�/B	�5B	�/B	�BB	�NB	�TB	�TB	�ZB	�`B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
bB
�B
%�B
/B
5?B
=qB
C�B
H�B
Q�B
W
B
YB
^5B
bNB
iyB
m�B
r�B
v�B
|�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BD�BD�BD�BD�BD�BD�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BE�BF�BG�BK�BL�BP�Bm�B�9B��BCB$�B%�B'�BWB�Bu�B{�B�B�YB�$B�B��B{�B��B��B��B��B�2B�kB�B�lB�xB�yB�kB�1B��B�XB�IB#�B�wB�B�B�B��B��B��B�qB�KBq�B`6BQ�BE�B:TB2!B�B
�BWBzBB
��B
��B
�XB
�MB
�0B
��B
�B
s�B
I�B
R�B
_3B
bDB
dPB
_1B
T�B
I�B
?tB
8IB
-B
!�B
vB
B	��B	�wB	��B	�"B	��B	��B	�B	gmB	_9B	Q�B	>vB	6DB	.B	�B	KB	B	B��B�B�B�|B�B��B��BÛB�wB�xB�iB�7B�B��B��B��B�|B�nB�xB�uB�zB�xB�jB�dB�UB�OB�CB�4B�3B�$B�&B�B{�By�Bw�Bs�Bq�Bv�Bx�By�Bx�B|�B�B�$B�#B}�Bj�B^<Bm�Br�Bp�Bo�Bn�Bj�BhzBhzBh{Bl�Bt�B�mB�$BØBƫBǯBĞBğB��B��B�WB�YB�oB�sB�zB�yB�B�B��B��B	B	0B	bB	�B	~B	�B	#�B	)�B	,B	(�B	"�B	�B	@B	B��B��B	AB	AB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�YB�B�B��B��B��B��B��B��B��B��B	B	B	B	B	+B	
:B	qB	B	�B	#�B	'�B	-
B	.B	6@B	=lB	I�B	J�B	K�B	L�B	C�B	:XB	C�B	D�B	A�B	8OB	3/B	5:B	9PB	D�B	K�B	WB	^.B	U�B	K�B	M�B	K�B	P�B	T�B	T�B	WB	WB	R�B	R�B	WB	ZB	^.B	`9B	gfB	q�B	u�B	y�B	{�B	{�B	{�B	y�B	u�B	u�B	w�B	|�B	�B	�0B	�JB	�@B	�MB	�3B	�B	}�B	u�B	m�B	m�B	o�B	u�B	��B	�B	�.B	�<B	�5B	�B	~�B	x�B	u�B	u�B	o�B	m�B	n�B	n�B	l�B	kB	f]B	cNB	dRB	f_B	l�B	o�B	p�B	v�B	��B	�\B	��B	��B	��B	��B	��B	�B	�B	�4B	�7B	�5B	�IB	�/B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�*B	�+B	�*B	�0B	�;B	�7B	�=B	�HB	�HB	�aB	�bB	�`B	�aB	�_B	�`B	�mB	�B	B	ËB	ƞB	ŘB	ŚB	ŗB	ƝB	ɲB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�&B	�<B	�=B	�/B	�$B	�+B	�#B	�6B	�CB	�HB	�IB	�NB	�UB	�qB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 B
 B
�B
�B
�B
 B
B
B
B
B
B
G�O�B
B
VB
�B
%�B
/B
52B
=bB
C�B
H�B
Q�B
V�B
Y	B
^(B
bBB
ihB
m�B
r�B
v�B
|�B
��B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436412016080714364120160807143641  AO  ARCAADJP                                                                    20151113201811    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151113201811  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151113201811  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143641  IP                  G�O�G�O�G�O�                