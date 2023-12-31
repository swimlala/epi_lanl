CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:52Z creation      
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041152  20190604094022  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @���]|�V1   @�����:H@37���+�dH�n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@��BG��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*�C,�C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyD�  D�<{D���D��)D�HD�J=D��fD��HD�
=D�@�D�|)D�ҏD�\D�eqDڈ D���D��D�D)D�w�D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B@��BGfgBOfgBW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!ٙC#�3C%�3C'�3C*�C,�C.�C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cv�Cx�Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DAvgDA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dtc3Dy�\D��fD�:�D�\D�ʏD��D�H�D���D���D��D�?
D�z�D���D��D�c�DچfD��GD��zD�B�D�vD��p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�=qA�5?A�1'A���A��A��yA��mA��`A��/A��#A���A���A���A���A���A���A���A�ĜAݼjAݺ^Aݩ�A�\)A�v�A�$�A�1'A�E�A��A��
A�  A�{A�+AсA�A�bA͏\A�+A���A�5?A�|�Aɛ�A�M�AȃAōPAĲ-A�JA�?}A�K�A��
A�"�A��!A��wA�9XA���A��^A�ƨA�E�A�{A�9XA��!A�v�A��A�v�A��#A�1'A��A�%A��FA�ffA��jA���A��A�&�A�~�A���A�ƨA�  A��jA�+A��A��^A�ZA�
=A���A��;A���A� �A��HA���A��A�K�A���A���A��A���A��FA�
=A���A�E�A��A�  A�M�A���A�K�A�n�A���A���A�  A�7LA�A�ĜA� �A���A�1A���A�1'A�A�A���A�C�A�A���A�1A���A��^AA}`BA|Q�Ay�Aw�wAvz�AsdZAp�Al��AhZAg�7AgS�Ae�mAcx�Aa�
A`bA_;dA^�+A]��A\�RA[&�AY�;AXA�AVv�AU�;ATVASVAQ�AO�7AMO�ALE�AJ��AG\)AD9XABv�A@v�A?�
A?�A<��A<1A;p�A9�A97LA8jA5��A3�FA2��A1�;A0��A.^5A-�A,�A++A(�9A&��A&�9A&JA%��A$ffA"A ��At�AM�A��A��A;dA�mAn�AA�DA�A�FA&�AA�AC�A��A��A��AhsAv�A?}A-A��A7LA��Ap�A�#A	dZAA��A�AZAJA�9AffA��A�TA M�@���@�J@��9@���@��P@�=q@�?}@���@�&�@�v�@�=q@�r�@�~�@���@�|�@�~�@�@�?}@�@�@�/@�w@���@�hs@�@��H@���@�hs@�\)@�{@��`@��@�V@�J@ҸR@�G�@�V@�(�@�C�@Ώ\@��@�^5@�;d@ΰ!@͡�@̬@�Q�@�bN@�ƨ@�n�@�`B@�X@�j@���@ǍP@�C�@���@�$�@�@�?}@�|�@�V@��@�;d@���@���@��@�?}@�I�@�$�@�&�@��@�J@�Q�@��`@�&�@��D@��`@��j@��u@��
@���@�E�@��@��#@��@���@� �@��
@��F@��@�C�@���@�v�@��@��-@�`B@��j@�r�@�9X@�  @��P@�K�@��@��@��!@�^5@�-@��@�O�@���@�Z@�1@���@��F@��@�\)@�S�@�+@�n�@��@��^@�7L@�V@��`@��@�Z@�  @��w@�l�@�dZ@�;d@��y@���@�^5@�-@���@�hs@�V@��j@��u@��@�Q�@�(�@��@��;@���@�@�V@�J@���@�X@��@�%@��@��@�A�@� �@�  @���@��F@��@���@��P@�t�@�\)@�33@���@���@�M�@�J@��#@���@�G�@��@��9@�b@��@��P@�|�@��@��H@�=q@�@��#@���@��h@�O�@��`@�1'@��@���@���@��@�33@�+@�33@�
=@��@�ff@��#@�@���@���@�G�@�V@��`@��@�O�@�p�@�x�@���@�z�@�I�@�1'@�b@�1@��;@��@���@�l�@�33@��H@��R@�ff@�E�@�E�@�v�@�^5@�^5@�E�@�J@�@�p�@�X@��@��@���@��@�j@�Z@�I�@�1@��w@�+@��!@�~�@�ff@�V@�$�@��@��-@��@�/@��`@��@�A�@��@��F@�|�@�@���@�V@�5?@�-@�@���@���@�l"@y�@q�C@f��@]�D@U%@M�d@E��@@U2@9��@3Z�@+��@%�)@ �/@��@+k@�X@*0@qv@'R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�=qA�5?A�1'A���A��A��yA��mA��`A��/A��#A���A���A���A���A���A���A���A�ĜAݼjAݺ^Aݩ�A�\)A�v�A�$�A�1'A�E�A��A��
A�  A�{A�+AсA�A�bA͏\A�+A���A�5?A�|�Aɛ�A�M�AȃAōPAĲ-A�JA�?}A�K�A��
A�"�A��!A��wA�9XA���A��^A�ƨA�E�A�{A�9XA��!A�v�A��A�v�A��#A�1'A��A�%A��FA�ffA��jA���A��A�&�A�~�A���A�ƨA�  A��jA�+A��A��^A�ZA�
=A���A��;A���A� �A��HA���A��A�K�A���A���A��A���A��FA�
=A���A�E�A��A�  A�M�A���A�K�A�n�A���A���A�  A�7LA�A�ĜA� �A���A�1A���A�1'A�A�A���A�C�A�A���A�1A���A��^AA}`BA|Q�Ay�Aw�wAvz�AsdZAp�Al��AhZAg�7AgS�Ae�mAcx�Aa�
A`bA_;dA^�+A]��A\�RA[&�AY�;AXA�AVv�AU�;ATVASVAQ�AO�7AMO�ALE�AJ��AG\)AD9XABv�A@v�A?�
A?�A<��A<1A;p�A9�A97LA8jA5��A3�FA2��A1�;A0��A.^5A-�A,�A++A(�9A&��A&�9A&JA%��A$ffA"A ��At�AM�A��A��A;dA�mAn�AA�DA�A�FA&�AA�AC�A��A��A��AhsAv�A?}A-A��A7LA��Ap�A�#A	dZAA��A�AZAJA�9AffA��A�TA M�@���@�J@��9@���@��P@�=q@�?}@���@�&�@�v�@�=q@�r�@�~�@���@�|�@�~�@�@�?}@�@�@�/@�w@���@�hs@�@��H@���@�hs@�\)@�{@��`@��@�V@�J@ҸR@�G�@�V@�(�@�C�@Ώ\@��@�^5@�;d@ΰ!@͡�@̬@�Q�@�bN@�ƨ@�n�@�`B@�X@�j@���@ǍP@�C�@���@�$�@�@�?}@�|�@�V@��@�;d@���@���@��@�?}@�I�@�$�@�&�@��@�J@�Q�@��`@�&�@��D@��`@��j@��u@��
@���@�E�@��@��#@��@���@� �@��
@��F@��@�C�@���@�v�@��@��-@�`B@��j@�r�@�9X@�  @��P@�K�@��@��@��!@�^5@�-@��@�O�@���@�Z@�1@���@��F@��@�\)@�S�@�+@�n�@��@��^@�7L@�V@��`@��@�Z@�  @��w@�l�@�dZ@�;d@��y@���@�^5@�-@���@�hs@�V@��j@��u@��@�Q�@�(�@��@��;@���@�@�V@�J@���@�X@��@�%@��@��@�A�@� �@�  @���@��F@��@���@��P@�t�@�\)@�33@���@���@�M�@�J@��#@���@�G�@��@��9@�b@��@��P@�|�@��@��H@�=q@�@��#@���@��h@�O�@��`@�1'@��@���@���@��@�33@�+@�33@�
=@��@�ff@��#@�@���@���@�G�@�V@��`@��@�O�@�p�@�x�@���@�z�@�I�@�1'@�b@�1@��;@��@���@�l�@�33@��H@��R@�ff@�E�@�E�@�v�@�^5@�^5@�E�@�J@�@�p�@�X@��@��@���@��@�j@�Z@�I�@�1@��w@�+@��!@�~�@�ff@�V@�$�@��@��-@��@�/@��`@��@�A�@��@��F@�|�@�@���@�V@�5?@�-@�G�O�@���@�l"@y�@q�C@f��@]�D@U%@M�d@E��@@U2@9��@3Z�@+��@%�)@ �/@��@+k@�X@*0@qv@'R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB>wB=qB=qB=qB=qB=qB<jB=qB<jB<jB=qB=qB=qB=qB<jB<jB;dB;dB;dB<jBD�B}�B��B��B��B��B�-B��B��B��B�B.B5?B7LB?}B?}B1'B'�B&�B7LB;dB8RBL�BdZBr�Bm�Bv�B�JB��B��B�!B�jBŢBɺB��B�5B�BB�BB�5B�#B�B��B��B�)B�NB�TB�;B�B�/B��B��B��BŢB�XB�FB�9B�-B�!B�B��B��B��B�uB�\B�PB�DB�B|�B�B�B~�Bq�B`BBT�BA�B#�B�B+B��B��B�B�`BƨB�RB��B��B��B��B��B�VB�BbNB>wB&�BB
�B
�FB
��B
�PB
q�B
^5B
K�B
7LB
-B
�B

=B	��B	�NB	��B	�LB	��B	��B	�uB	�+B	v�B	t�B	�7B	�VB	�PB	�7B	�B	w�B	m�B	e`B	ZB	T�B	J�B	A�B	8RB	-B	�B	�B	bB	  B�B�sB�TB�;B�/B�#B�
B��B��BƨBB�wB�dB�RB�FB�-B�B�B��B��B��B��B��B�{B�oB�PB�1B�B�B�B� B�B�B�uB��B��B��B�HB�;B�/B�HB�;B�)B�#B�B�B��B��BƨB�qB�^B�LB�-B��B��B�uB�=B�B|�B~�Bz�Bz�B{�B~�B~�B~�B}�B}�B�+B�1B�+B�7B�VB�{B��B��B��B��B��B�{B�uB�oB�hB�hB�oB�hB�JB�1B��B��B��B��B�oB�VB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�RB�jB�wBBĜB��B��B��B��B��B�
B�#B�;B�ZB�sB�B��B��B��B��B��B�B�B��B	\B	hB	hB	\B	DB		7B	%B	B	PB	\B	VB	
=B	DB	\B	{B	�B	�B	 �B	"�B	"�B	"�B	$�B	%�B	%�B	&�B	'�B	'�B	)�B	,B	-B	.B	2-B	5?B	6FB	7LB	;dB	<jB	>wB	?}B	@�B	C�B	D�B	F�B	K�B	O�B	Q�B	S�B	VB	VB	W
B	XB	XB	ZB	_;B	`BB	bNB	e`B	ffB	iyB	k�B	m�B	o�B	p�B	s�B	t�B	u�B	v�B	w�B	y�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�jB	�jB	�qB	�wB	��B	��B	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�5B	�5B	�5B	�BB	�HB	�HB	�HB	�HB	�BB	�BB	�HB	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
+B
+B
1B
	7B
	7B
DB
vB
�B
WB
,�B
49B
<�B
>�B
G�B
K�B
S[B
YeB
^�B
d&B
i�B
mB
p!B
u?B
xB
}�B
��B
�g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B<_B<[B<\B;WB;UB;WB;WB;WB:MB;UB:RB:RB;WB;WB;YB;WB:OB:SB9KB9JB9EB:QBB�B{�B��B��B��B��B�B�fB��B��BqB+�B3#B50B=_B=ZB/B%�B$�B50B9HB63BJ�Bb=Bp�BkrBt�B�)B�sB��B�B�LBÃBǛB��B�B�$B�&B�B�B��B��B��B�B�0B�4B�B��B�B��BʳB��BÀB�9B�)B�B�B�B��B��B��B��B�YB�>B�3B�'B~�Bz�B�B�B|�Bo�B^&BR�B?nB!�BgBB��B��B�B�FBĒB�9B��B��B��B��B�oB�>B��B`8B<`B$�B
��B
��B
�2B
��B
�:B
o�B
\$B
I�B
54B
*�B
�B
/B	��B	�<B	ɷB	�=B	��B	�|B	�jB	�B	t�B	r�B	�%B	�DB	�BB	�(B	�B	u�B	k�B	cRB	XB	R�B	H�B	?{B	6FB	+B	�B	�B	ZB��B�B�gB�JB�.B�%B�B�B��BɽBĞB��B�mB�ZB�IB�;B�%B�B��B��B��B��B��B��B�qB�gB�HB�(B�B�B~�B}�B�	B�B�mB��B��B�wB�AB�0B�)B�@B�1B�#B�B�B�
B��B��BğB�eB�SB�AB�$B��B��B�nB�4B�Bz�B|�Bx�Bx�By�B|�B|�B|�B{�B{�B�&B�'B�'B�.B�NB�tB��B��B��B��B�B�pB�nB�hB�bB�_B�iB�aB�DB�+B��B��B��B��B�fB�PB�iB�uB�B��B��B��B��B��B��B��B��B��B�B�JB�eB�qB��BBȺB��B��B��B��B�B�B�5B�TB�kB�B��B��B��B��B��B�B�B��B	WB	_B	`B	QB		9B	.B	B	B	IB	WB	PB	3B		;B	UB	sB	�B	�B	�B	 �B	 �B	 �B	"�B	#�B	#�B	$�B	%�B	%�B	'�B	*B	+B	,B	0#B	37B	4@B	5BB	9YB	:`B	<oB	=yB	>{B	A�B	B�B	D�B	I�B	M�B	O�B	Q�B	S�B	S�B	UB	VB	VB	XB	]4B	^;B	`FB	cYB	d[B	gpB	iyB	k�B	m�B	n�B	q�B	r�B	s�B	t�B	u�B	w�B	y�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�2B	�AB	�UB	�oB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�+B	�0B	�.B	�8B	�<B	�BB	�MB	�MB	�TB	�ZB	�\B	�^B	�`B	�iB	�mB	�zB	�~B	B	×B	ƪB	ȸB	ɾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�*B	�,B	�*B	�8B	�>B	�=B	�:B	�?B	�3B	�2B	�<B	�TB	�ZB	�hB	�pB	�lB	�vB	�vB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
 B
	B
B
B
B
 B
!B
'B
-B
.G�O�B
lB
�B
KB
*�B
2.B
:�B
<�B
E�B
I�B
QMB
WZB
\�B
bB
g�B
kB
nB
s4B
u�B
{�B
|B
�Y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.002(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940222019060409402220190604094022  AO  ARCAADJP                                                                    20181121041152    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041152  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041152  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094022  IP                  G�O�G�O�G�O�                