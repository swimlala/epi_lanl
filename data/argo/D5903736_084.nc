CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-31T00:02:52Z AOML 3.0 creation; 2016-05-31T19:14:38Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140731000252  20160531121438  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               TA   AO  4051_7090_084                   2C  D   APEX                            5368                            041511                          846 @��;Z��1   @������@2��
=p��d�?|�h1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    TA   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBF��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDx��D���D�6fD�� D���D��D�VfD�p D�ٚD��D�C3D���Dǹ�D�	�D�0 D�fD��3D� D�9�D�i�D�P 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @0  @|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B@33BF��BOfgBW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D�gDvgD��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'�gD(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dtc3Dx�gD��3D�4�D�~fD��3D� D�T�D�nfD�� D�3D�A�D��3DǸ D� D�.fD��D�љD�fD�8 D�h D�Nf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�9XA�5?A�5?A�7LA�7LA�5?A�7LA�5?A�5?A�7LA�33A�5?A�5?A�1'A��A��A��A��A� �A��A�ƨA޼jA޸RA�=qA� �AԸRAӗ�A�A�/A�&�A�~�A��`A�%A�AɮA���A�A���A���A��7A��A�oA�7LA�&�A��A�bA���A��A�n�A�oA�v�A��A��\A��!A��A�E�A���A�^5A��
A���A�\)A���A��-A�A�I�A���A�$�A���A�~�A�+A��A�9XA�dZA���A���A�O�A�^5A�ȴA�&�A���A�l�A�ƨA��HA�%A�-A��DA��A�t�A�n�A��-A�JA��uA���A��-A��wA��jA�`BA��`A�VA��`A�A��A�E�A��A�ĜA���A�JA�jA��yA��PA��A�wA|�HAw�Au��As��Ar��Aq��Ap�Ao�FAi�PAdI�AcA`{A\�AZ�DAX�/AW"�AUG�AS��AP�yAO33AM��AM+ALffAJI�AGC�AE��AD��AD{AC��ACO�AAoA?\)A?"�A>�A>�A< �A:�`A:ZA97LA8~�A7�FA7%A5ƨA4��A3A0ffA-�7A,jA+�A*jA(��A'�A&��A$jA#
=A!��A�TA�A�AhsA�/At�A��A�-A+AAz�A�
A`BAE�A
=A~�A��A�A^5A�A`BA+A��A7LA\)A�7A
��A
�A	t�A	hsA�!A�TAXA�HA�\AI�AƨA�`A=qA�AhsAG�AĜA��@��
@�ȴ@�ff@�`B@��@���@��D@��
@�@��u@���@�/@�V@�?}@���@�D@�  @�ƨ@��;@���@�ƨ@@�bN@��@�F@畁@��@�R@�
=@�E�@�@���@��y@�O�@��@���@�I�@�ƨ@�\)@���@��#@�Ĝ@� �@�v�@ׅ@�;d@�C�@���@��@�X@�X@�z�@Ѻ^@ӥ�@��y@�S�@�  @�9X@�v�@�^5@Ѻ^@�O�@�o@��y@�7L@�&�@�9X@Ϯ@�
=@���@�-@�1@�z�@�@�ȴ@�I�@�
=@��@��^@�  @�V@� �@�z�@ȃ@Ȭ@ȼj@���@��/@��m@��@�p�@�-@�ff@�@�\)@��@�
=@�;d@�o@�Q�@�E�@�n�@�@���@��j@���@�@���@���@��\@�&�@���@��F@��y@��@�(�@�bN@��\@�7L@�V@�@���@���@�l�@��`@�p�@�?}@�+@�M�@��R@��@��7@�X@��@���@��@�S�@��@��@��@�n�@��T@��7@�/@���@��m@��F@���@�  @�A�@� �@��@�1@�ƨ@�\)@�dZ@��@���@���@�~�@�V@���@���@�hs@�G�@��`@�  @�K�@��y@�V@�/@�A�@���@�\)@�"�@���@��#@���@���@�\)@�dZ@��P@�t�@��H@�v�@�~�@�~�@�X@�Q�@��
@���@�dZ@�33@��@��H@�ff@��@�x�@��@���@���@�z�@�A�@� �@��;@�l�@�"�@���@��@��T@��^@���@�X@��@���@���@��@��@�b@�t�@�"�@��H@��R@�V@�@��@���@���@���@�@��@��D@�Z@�S�@���@�`B@�V@�1'@�  @��@�z�@�9X@���@��@��@�@�ff@�V@���@�x�@�hs@�?}@�V@��`@���@���@�Ĝ@�Ĝ@���@��u@��@�1'@��m@�+@�E�@�$�@��T@��h@��@�V@��@�Ĝ@��D@�bN@�Z@�9X@�~�@�{@~��@u�@o��@f{@Z~�@Ol�@Cƨ@=p�@5�T@0b@+33@'l�@ �@�@�`@1@�@�D@	�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�;dA�9XA�5?A�5?A�7LA�7LA�5?A�7LA�5?A�5?A�7LA�33A�5?A�5?A�1'A��A��A��A��A� �A��A�ƨA޼jA޸RA�=qA� �AԸRAӗ�A�A�/A�&�A�~�A��`A�%A�AɮA���A�A���A���A��7A��A�oA�7LA�&�A��A�bA���A��A�n�A�oA�v�A��A��\A��!A��A�E�A���A�^5A��
A���A�\)A���A��-A�A�I�A���A�$�A���A�~�A�+A��A�9XA�dZA���A���A�O�A�^5A�ȴA�&�A���A�l�A�ƨA��HA�%A�-A��DA��A�t�A�n�A��-A�JA��uA���A��-A��wA��jA�`BA��`A�VA��`A�A��A�E�A��A�ĜA���A�JA�jA��yA��PA��A�wA|�HAw�Au��As��Ar��Aq��Ap�Ao�FAi�PAdI�AcA`{A\�AZ�DAX�/AW"�AUG�AS��AP�yAO33AM��AM+ALffAJI�AGC�AE��AD��AD{AC��ACO�AAoA?\)A?"�A>�A>�A< �A:�`A:ZA97LA8~�A7�FA7%A5ƨA4��A3A0ffA-�7A,jA+�A*jA(��A'�A&��A$jA#
=A!��A�TA�A�AhsA�/At�A��A�-A+AAz�A�
A`BAE�A
=A~�A��A�A^5A�A`BA+A��A7LA\)A�7A
��A
�A	t�A	hsA�!A�TAXA�HA�\AI�AƨA�`A=qA�AhsAG�AĜA��@��
@�ȴ@�ff@�`B@��@���@��D@��
@�@��u@���@�/@�V@�?}@���@�D@�  @�ƨ@��;@���@�ƨ@@�bN@��@�F@畁@��@�R@�
=@�E�@�@���@��y@�O�@��@���@�I�@�ƨ@�\)@���@��#@�Ĝ@� �@�v�@ׅ@�;d@�C�@���@��@�X@�X@�z�@Ѻ^@ӥ�@��y@�S�@�  @�9X@�v�@�^5@Ѻ^@�O�@�o@��y@�7L@�&�@�9X@Ϯ@�
=@���@�-@�1@�z�@�@�ȴ@�I�@�
=@��@��^@�  @�V@� �@�z�@ȃ@Ȭ@ȼj@���@��/@��m@��@�p�@�-@�ff@�@�\)@��@�
=@�;d@�o@�Q�@�E�@�n�@�@���@��j@���@�@���@���@��\@�&�@���@��F@��y@��@�(�@�bN@��\@�7L@�V@�@���@���@�l�@��`@�p�@�?}@�+@�M�@��R@��@��7@�X@��@���@��@�S�@��@��@��@�n�@��T@��7@�/@���@��m@��F@���@�  @�A�@� �@��@�1@�ƨ@�\)@�dZ@��@���@���@�~�@�V@���@���@�hs@�G�@��`@�  @�K�@��y@�V@�/@�A�@���@�\)@�"�@���@��#@���@���@�\)@�dZ@��P@�t�@��H@�v�@�~�@�~�@�X@�Q�@��
@���@�dZ@�33@��@��H@�ff@��@�x�@��@���@���@�z�@�A�@� �@��;@�l�@�"�@���@��@��T@��^@���@�X@��@���@���@��@��@�b@�t�@�"�@��H@��R@�V@�@��@���@���@���@�@��@��D@�Z@�S�@���@�`B@�V@�1'@�  @��@�z�@�9X@���@��@��@�@�ff@�V@���@�x�@�hs@�?}@�V@��`@���@���@�Ĝ@�Ĝ@���@��u@��@�1'@��m@�+@�E�@�$�@��T@��h@��@�V@��@�Ĝ@��D@�bN@�Z@�9X@�~�@�{@~��@u�@o��@f{@Z~�@Ol�@Cƨ@=p�@5�T@0b@+33@'l�@ �@�@�`@1@�@�D@	�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B�B��B33B=qBhsB�7B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�B��B��B��B�uB�B� B|�Bu�B^5BK�BB�B)�B�B�B�B�B�BoB
=B��B�B�#BB��Bm�BL�B(�BDB�B�/B�qB��B�PB�Bu�BbNBE�B-B$�B�BuBB
�fB
�BB
�B
��B
ȴB
�dB
�!B
��B
��B
��B
��B
�B
y�B
q�B
l�B
ffB
aHB
K�B
.B
�B
{B
JB
B	��B	�B	��B	�-B	��B	��B	�DB	�B	w�B	m�B	dZB	]/B	VB	Q�B	K�B	H�B	C�B	:^B	0!B	)�B	%�B	"�B	 �B	�B	{B	VB	JB	
=B	B��B��B��B�B�B�sB�`B�NB�;B�B��BȴBĜB��B�qB�XB�FB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�-B�3B�-B�B�'B�LB�qB�dB�^B�qB��BǮBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBȴBǮBŢBŢBŢBŢBŢBĜBÖBB��B��B��B�}BŢBȴB��B��B��B�
B�B�B�B�B��B��BŢBĜBƨB��B��B�B�5B�B��BɺBǮBB��B�qB�^B�LB�-B�'B�3B�wB��B��B��B��B�B�`B�TB�fB	B	bB	�B	 �B	%�B	,B	.B	'�B	/B	1'B	6FB	E�B	G�B	C�B	H�B	K�B	K�B	I�B	C�B	5?B	,B	 �B	�B	�B	�B	�B	 �B	1'B	D�B	VB	aHB	dZB	dZB	ffB	jB	hsB	`BB	[#B	T�B	]/B	cTB	dZB	`BB	\)B	M�B	M�B	O�B	Q�B	ZB	dZB	ffB	k�B	ffB	aHB	]/B	W
B	YB	ZB	e`B	dZB	`BB	cTB	x�B	x�B	q�B	s�B	n�B	hsB	aHB	iyB	w�B	�B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�3B	�-B	�'B	�'B	�'B	�-B	�'B	�!B	�B	�!B	�9B	�LB	�RB	�RB	�XB	�^B	�qB	�}B	��B	ÖB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	ȴB	ƨB	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�#B	�)B	�)B	�5B	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
�B
 �B
(�B
,B
2-B
;dB
B�B
J�B
O�B
VB
[#B
^5B
aHB
e`B
iyB
m�B
r�B
v�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B��B��B��B�B��B3<B=yBh}B�AB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B�$B�EB�B��B��B��B�~B�(B�B|�Bu�B^;BK�BB�B*B�B�B�B�B�BwB
CB��B�B�+BB��Bm�BL�B(�BEB�B�2B�xB��B�UB�Bu�BbVBE�B-B$�B�B|BB
�lB
�HB
�!B
��B
��B
�mB
�)B
��B
��B
��B
��B
�B
y�B
q�B
l�B
fqB
aTB
K�B
."B
�B
�B
WB
,B	��B	�B	��B	�>B	�B	��B	�UB	�B	w�B	m�B	dmB	]DB	VB	Q�B	K�B	H�B	C�B	:pB	06B	*B	%�B	"�B	 �B	�B	�B	lB	`B	
QB	3B�B��B��B�B�B�B�xB�gB�SB�.B��B��BķB��B��B�qB�`B�GB�/B�B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�3B�4B�3B�;B�@B�HB�MB�DB�4B�AB�dB��B�{B�xB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BźBŹBŻBżBŹBĴBíB¨B��B��B��B��BźB��B��B��B��B�"B�0B�4B�4B�B�B��BźBĴBƿB��B�B�0B�OB�*B��B��B��BªB��B��B�xB�gB�FB�@B�LB��B�B��B�B��B�B�vB�lB�B	.B	vB	�B	 �B	%�B	,B	.)B	(B	/.B	1<B	6XB	E�B	G�B	C�B	H�B	K�B	K�B	I�B	C�B	5QB	,B	 �B	�B	�B	�B	�B	 �B	18B	D�B	VB	aZB	dlB	djB	fwB	j�B	h�B	`TB	[4B	UB	]@B	cgB	dnB	`SB	\:B	M�B	M�B	O�B	Q�B	Z0B	dkB	fxB	k�B	fxB	aZB	]@B	WB	Y)B	Z0B	eqB	dmB	`SB	ceB	x�B	x�B	q�B	s�B	n�B	h�B	a[B	i�B	w�B	�B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�>B	�CB	�DB	�>B	�7B	�7B	�9B	�<B	�6B	�0B	�-B	�1B	�GB	�]B	�aB	�aB	�fB	�mB	��B	��B	��B	æB	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƸB	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�!B	�"B	�#B	�%B	�$B	�%B	�%B	�.B	�,B	�7B	�8B	�8B	�6B	�7B	�8B	�8B	�2B	�6B	�8B	�DB	�PB	�WB	�VB	�[B	�bB	�iB	�jB	�nB	�oB	�uB	�tB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
B
B
�B
 �B
)B
,B
28B
;nB
B�B
J�B
O�B
VB
[/B
^>B
aRB
emB
i�B
m�B
r�B
v�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214382016053112143820160531121438  AO  ARCAADJP                                                                    20140731000252    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140731000252  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140731000252  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121438  IP                  G�O�G�O�G�O�                