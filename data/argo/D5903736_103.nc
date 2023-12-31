CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-10T10:15:58Z AOML 3.0 creation; 2016-05-31T19:14:41Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150210101558  20160531121441  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               gA   AO  4051_7090_103                   2C  D   APEX                            5368                            041511                          846 @�9Aܺ�1   @�9B~�@4�V�u�ddj~��#1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    gA   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtL�Dy��D�3D�0 D�l�D���D� D�9�D��fD�ɚD���D�,�D��fD�� D�3D�@ Dډ�D�ɚD�  D�FfD�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @;�@���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
RD
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"�RD#RD#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�DtN�Dy��D�)D�0�D�m�D���D��D�:�D��\D�ʐD���D�-�D��\D���D�)D�@�Dڊ�D�ʐD� �D�G\D�d)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A���A���A�ƨA�A�A¼jA¼jA¼jA¾wA¼jA¸RA¶FA¸RA¶FA¶FA¶FA¶FA¸RA¶FA¶FA¶FA´9A®A®A¬A¬A¬A¬A¬A©�A©�A©�A£�A¡�A�ADA�bNA�?}A��HA�l�A��HA��A��9A�K�A�9XA�33A�VA��A��HA���A���A�7LA�t�A�G�A�-A���A��uA�JA�n�A���A��-A�l�A�%A��
A�I�A�~�A���A��A���A���A��A�C�A��jA��PA�r�A�C�A���A��
A�dZA���A��A��A�\)A�{A��FA��`A�r�A�
=A�  A���A���A�ĜA�z�A��DA��-A��PA��A�I�A���A�A�\)A���A���A�A�A���A���A��-A��#A�`BA��PA�XA�A�=qA}�mAz~�Aw��Av$�As�ArjApffAk��Ai��AiVAe��Ab�HA`��A`5?A^$�A\5?AX�RAV�AUt�AS�TAR~�AQ|�AP�HAP^5AO��AN�yAM`BAK�^AH��AGhsAFE�AD�ADE�AD �ADbAD  ACƨAC�hACt�AB�9A@��A?dZA=%A;��A9�;A6�A4�!A3XA17LA/�A,ĜA+`BA+�A+
=A*�`A*�jA*r�A*{A)�A)�FA)/A(ĜA(z�A((�A'|�A'
=A&�A&z�A%�7A#��A"n�A"1A!�7A Q�A��A�Ax�A��AE�A�wA�yA�wA�A$�AO�AQ�A+A=qAO�A��AZA|�A-AAp�A�!A|�A
��A	��A	XA	oA�jA�uAA�9A��A��AdZA%A$�AA �yA n�@���@�O�@�r�@�|�@�M�@�`B@���@��R@�A�@��@�5?@�Ĝ@��@��@���@�p�@�G�@�Ĝ@��@�ȴ@�t�@�V@�O�@���@�u@�h@�Q�@�  @ߥ�@�
=@�hs@�9X@ڇ+@�&�@أ�@�Q�@���@���@���@�7L@ԛ�@���@ҧ�@�O�@��@�o@���@·+@�ȴ@���@�ff@��`@��@�M�@ȼj@�ƨ@��@�O�@ļj@��@å�@�@�@���@��h@��@�I�@��;@��H@��@�%@�A�@��F@�dZ@�+@���@��@��@��-@�`B@�%@�Z@�o@�ff@���@�?}@��/@��j@��u@�j@��@���@��P@��@��+@�=q@��^@�x�@�?}@��@���@�Ĝ@�j@��w@�l�@�@�E�@�J@��@���@�x�@��9@�r�@�A�@��w@�dZ@�33@�"�@��@��@�@��y@�ȴ@��\@�M�@��@��#@��^@���@��7@�/@�%@��@�Q�@��F@���@�l�@�33@�+@��R@�5?@���@��@�=q@�E�@�5?@�-@�J@�@�G�@��`@��D@�9X@��@�;d@��@�v�@�$�@�x�@��@�%@���@�1'@�|�@�\)@�;d@�33@��@���@���@�V@���@��h@��@���@���@���@�9X@��@��F@�33@�o@���@��@�hs@�O�@�7L@���@��`@��9@���@�z�@�A�@�1@�ƨ@�dZ@�K�@�;d@�+@���@���@���@�hs@�O�@�%@��j@�Q�@�1@�l�@�@��@�~�@�-@��^@�/@���@��@���@���@�Z@��m@��w@��P@�"�@�
=@���@���@��!@���@���@�ff@�-@�$�@�J@��@��@�p�@��j@��@�j@�Q�@��@�ƨ@�K�@���@�$�@���@�p�@�X@��@���@��@�Z@�9X@��@���@��F@��
@��F@�S�@��9@~�@xbN@o;d@fV@]��@V$�@Ol�@G��@A��@:M�@4�/@/|�@)7L@$j@;d@�@$�@~�@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A���A���A�ƨA�A�A¼jA¼jA¼jA¾wA¼jA¸RA¶FA¸RA¶FA¶FA¶FA¶FA¸RA¶FA¶FA¶FA´9A®A®A¬A¬A¬A¬A¬A©�A©�A©�A£�A¡�A�ADA�bNA�?}A��HA�l�A��HA��A��9A�K�A�9XA�33A�VA��A��HA���A���A�7LA�t�A�G�A�-A���A��uA�JA�n�A���A��-A�l�A�%A��
A�I�A�~�A���A��A���A���A��A�C�A��jA��PA�r�A�C�A���A��
A�dZA���A��A��A�\)A�{A��FA��`A�r�A�
=A�  A���A���A�ĜA�z�A��DA��-A��PA��A�I�A���A�A�\)A���A���A�A�A���A���A��-A��#A�`BA��PA�XA�A�=qA}�mAz~�Aw��Av$�As�ArjApffAk��Ai��AiVAe��Ab�HA`��A`5?A^$�A\5?AX�RAV�AUt�AS�TAR~�AQ|�AP�HAP^5AO��AN�yAM`BAK�^AH��AGhsAFE�AD�ADE�AD �ADbAD  ACƨAC�hACt�AB�9A@��A?dZA=%A;��A9�;A6�A4�!A3XA17LA/�A,ĜA+`BA+�A+
=A*�`A*�jA*r�A*{A)�A)�FA)/A(ĜA(z�A((�A'|�A'
=A&�A&z�A%�7A#��A"n�A"1A!�7A Q�A��A�Ax�A��AE�A�wA�yA�wA�A$�AO�AQ�A+A=qAO�A��AZA|�A-AAp�A�!A|�A
��A	��A	XA	oA�jA�uAA�9A��A��AdZA%A$�AA �yA n�@���@�O�@�r�@�|�@�M�@�`B@���@��R@�A�@��@�5?@�Ĝ@��@��@���@�p�@�G�@�Ĝ@��@�ȴ@�t�@�V@�O�@���@�u@�h@�Q�@�  @ߥ�@�
=@�hs@�9X@ڇ+@�&�@أ�@�Q�@���@���@���@�7L@ԛ�@���@ҧ�@�O�@��@�o@���@·+@�ȴ@���@�ff@��`@��@�M�@ȼj@�ƨ@��@�O�@ļj@��@å�@�@�@���@��h@��@�I�@��;@��H@��@�%@�A�@��F@�dZ@�+@���@��@��@��-@�`B@�%@�Z@�o@�ff@���@�?}@��/@��j@��u@�j@��@���@��P@��@��+@�=q@��^@�x�@�?}@��@���@�Ĝ@�j@��w@�l�@�@�E�@�J@��@���@�x�@��9@�r�@�A�@��w@�dZ@�33@�"�@��@��@�@��y@�ȴ@��\@�M�@��@��#@��^@���@��7@�/@�%@��@�Q�@��F@���@�l�@�33@�+@��R@�5?@���@��@�=q@�E�@�5?@�-@�J@�@�G�@��`@��D@�9X@��@�;d@��@�v�@�$�@�x�@��@�%@���@�1'@�|�@�\)@�;d@�33@��@���@���@�V@���@��h@��@���@���@���@�9X@��@��F@�33@�o@���@��@�hs@�O�@�7L@���@��`@��9@���@�z�@�A�@�1@�ƨ@�dZ@�K�@�;d@�+@���@���@���@�hs@�O�@�%@��j@�Q�@�1@�l�@�@��@�~�@�-@��^@�/@���@��@���@���@�Z@��m@��w@��P@�"�@�
=@���@���@��!@���@���@�ff@�-@�$�@�J@��@��@�p�@��j@��@�j@�Q�@��@�ƨ@�K�@���@�$�@���@�p�@�X@��@���@��@�Z@�9X@��@���@��F@��
@��F@�S�@��9@~�@xbN@o;d@fV@]��@V$�@Ol�@G��@A��@:M�@4�/@/|�@)7L@$j@;d@�@$�@~�@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB:^B:^B:^B:^B9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B;dB;dB;dB;dB;dB;dB;dB;dB<jB<jB<jB<jB<jB<jB:^B9XB7LB7LB;dB<jB@�BD�BF�BN�BW
BZB_;BaHB_;B_;B]/BZBW
BW
BVBR�BN�BJ�BE�B-B-B%�B�B�BJB��B�B�fB�NB�HB�BB�/B�
B��B�jB�?B�B��B��B��B�{Bz�BgmBaHB`BBP�BB�B6FB1'B�B+B�B�NB��B�LB��B��B{�B_;BS�B7LB{BB
�HB
ǮB
�B
�DB
�B
x�B
aHB
F�B
1'B
!�B
JB
B	�B	��B	�}B	�FB	��B	�JB	}�B	u�B	gmB	ZB	H�B	<jB	49B	,B	$�B	 �B	�B	�B	�B	oB	DB	B��B��B�B�B�B�B�B�B�B�B�B�mB�TB�5B�B��B��BȴBĜB��B�dB�FB�-B�!B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�\B�PB�PB�JB�DB�7B�7B�+B�+B�%B�B�B�B�B� B~�B}�B|�B{�Bz�Bx�Bw�Bv�Bv�Bv�Bu�Bu�Bt�Bs�Bs�Bs�Br�Br�Bq�Bo�Bn�Bo�Bo�Bo�Bq�Bp�Bp�Bq�Bq�Bq�Bq�Br�Br�Bs�Bt�Bv�Bw�Bw�Bx�Bw�Bw�Bw�Bx�B~�B� B�B�B� B�%B�1B�1B�7B�DB�\B�\B�hB��B��B��B��B��B��B��B��B��B�B�!B�'B�9B�FB�}BB��BBĜBŢB��B��B��B��B��B��B�B�B�)B�BB�HB�NB�`B�B�B�B��B��B	  B	B	B	B	+B	
=B	DB	JB	PB	VB	bB	uB	�B	�B	�B	 �B	!�B	!�B	#�B	(�B	+B	/B	2-B	5?B	6FB	8RB	9XB	:^B	>wB	?}B	A�B	E�B	K�B	M�B	O�B	P�B	R�B	R�B	S�B	T�B	ZB	^5B	`BB	aHB	e`B	ffB	gmB	hsB	iyB	iyB	jB	k�B	n�B	s�B	t�B	v�B	x�B	x�B	y�B	{�B	|�B	� B	�B	�%B	�+B	�7B	�VB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�9B	�3B	�-B	�-B	�3B	�9B	�FB	�RB	�^B	�dB	�dB	�wB	��B	B	ĜB	ƨB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�`B	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
DB
JB
PB
hB
�B
#�B
)�B
/B
49B
;dB
A�B
F�B
J�B
O�B
S�B
YB
_;B
cTB
hsB
l�B
q�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B:bB:_B:_B:_B9WB:_B:_B:_B:_B:_B:bB:_B:bB:_B:_B:bB:_B:_B:bB:_B:_B:_B:_B:]B:bB:aB:^B;eB;cB;eB;eB;eB;cB;cB;cB<kB<hB<kB<nB<mB<lB:`B9XB7MB7OB;eB<kB@�BD�BF�BN�BW
BZB_=BaKB_=B_>B]1BZBW
BW	BVBR�BN�BJ�BE�B-B-B%�B�B�BMB��B�B�eB�NB�DB�AB�,B�
B��B�iB�?B�B��B��B��B�vBz�BgkBaAB`?BP�BB�B6GB1%B�B&B�B�KB��B�KB��B��B{�B_9BS�B7KB{B
B
�JB
ǭB
�B
�EB
�B
x�B
aIB
F�B
1+B
!�B
MB
B	�B	��B	��B	�MB	��B	�SB	}�B	u�B	gyB	Z(B	H�B	<tB	4FB	,B	$�B	 �B	�B	�B	�B	{B	QB	&B��B��B�B�B�B�B�B�B�B�B�B�{B�dB�CB�(B�B��B��BĭB��B�sB�WB�>B�2B�+B�,B�+B�&B�B� B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�oB�cB�aB�\B�UB�IB�IB�>B�=B�8B�0B�-B�B� B�BB~B|�B{�Bz�Bx�Bw�Bv�Bv�Bv�Bu�Bu�Bt�Bs�Bs�Bs�Br�Br�Bq�Bo�Bn�Bo�Bo�Bo�Bq�Bp�Bp�Bq�Bq�Bq�Bq�Br�Br�Bs�Bt�Bv�Bw�Bw�Bx�Bw�Bw�Bw�Bx�BB�B�B�B�B�7B�BB�BB�HB�UB�mB�nB�xB��B��B��B��B��B��B��B��B��B�B�1B�6B�HB�XB��BB��BBĬBŲB��B��B��B��B��B�B�B�#B�6B�PB�TB�[B�kB�B�B�B��B��B	 
B	B	&B	$B	6B	
FB	NB	TB	ZB	aB	lB	B	�B	�B	�B	 �B	!�B	!�B	#�B	(�B	+
B	/$B	25B	5IB	6OB	8[B	9bB	:hB	>�B	?�B	A�B	E�B	K�B	M�B	O�B	P�B	R�B	R�B	TB	UB	Z&B	^;B	`HB	aPB	eeB	flB	guB	hzB	i�B	i�B	j�B	k�B	n�B	s�B	t�B	v�B	x�B	x�B	y�B	{�B	|�B	�B	�B	�)B	�2B	�=B	�\B	�uB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�5B	�8B	�:B	�>B	�9B	�2B	�3B	�8B	�<B	�LB	�SB	�cB	�mB	�kB	�zB	��B	B	ģB	ƫB	ǲB	��B	��B	ɿB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�'B	�+B	�4B	�4B	�9B	�>B	�FB	�FB	�LB	�QB	�PB	�QB	�VB	�bB	�pB	�vB	�xB	�wB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B
	B
B
B
B
B
B
B
B
B
B
B
B
B
!B
B
B
B
B
B
B
B
'B
(B
-B
.B
2B
	8B
IB
LB
RB
jB
�B
#�B
)�B
/B
4<B
;fB
A�B
F�B
J�B
O�B
S�B
YB
_=B
cTB
htB
l�B
q�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.03 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214412016053112144120160531121441  AO  ARCAADJP                                                                    20150210101558    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150210101558  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20150210101558  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121441  IP                  G�O�G�O�G�O�                