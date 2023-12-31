CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:05Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170905  20220204114415  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               1A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؛\�g6�1   @؛]ffu@6�O�;d�cܓt�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    1A   B   B   @���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|�C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�fD�Z�D���D���D�
D�R=D���D��fD�%qD�W�D���D��fD�)D�Z�Dڃ�D���D�{D�Z=D��D�θ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�ff@�ffA33A?33A`��A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��3B��fB��fB��B��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5ٙC7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cx�Cy�3C|�C}�3C�3C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"�3D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D63D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;�gD<vgD<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DT3DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt�3Dy��D��D�YGD��=D��RD�pD�P�D��
D���D�#�D�VD���D���D��D�YGDڂ=D��\D�
�D�X�D�)D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AǼjAǸRAǉ7A��A�dZA�{A��TA�x�AľwAĥ�Aġ�Aĝ�Aĕ�AčPAċDAąA�jA�Q�AþwA��A��`A���A®AuA�ffA�^5A�K�A�-A�
=A��A��mA��#A���A���A��A�=qA��wA�ffA��jA��-A�l�A���A�-A�\)A�ZA�ffA�A�\)A���A��+A�l�A� �A���A��A�ȴA��RA�l�A��^A�l�A�=qA���A�G�A��A�%A��`A���A��uA�^5A��jA�jA�ffA��A�bA�n�A��A��/A���A�(�A��mA�I�A�?}A�
=A��9A���A�^5A���A�A�ffA�bA���A�E�A��A��wA�Q�A���A�{A�`BA��/A�^5A���A���A��7A��RA�M�A�oA�O�A���A��A��A�oA~�/A{�Ay�Ax�AxQ�Aw�AuK�At��AsK�Ar�\Aqp�Ap^5AnI�Am�Al�Ak�wAjv�Ah�!AhbAg|�Af��AfjAd�yAb^5A`1A\ĜA[O�AZv�AY��AY7LAX�yAX��AW�AV��AS�AOx�ANȴAN~�AM��AJz�AH�RAHI�AF9XAC�TACG�AB�yAB��ABffAA�
AAO�A<ĜA9��A9�A7C�A4A3%A1��A/�A-�7A+�wA)dZA)�A'7LA$�A#oA"I�A!��A!��A!�FA VA�A%AO�AdZAJAz�A�A�A�A��AVAz�A�
A"�A�!A��A
ffA	7LA�^A|�A�Av�A=qA��AC�Ar�A�A��A ��@��
@���@�@�bN@��R@���@�&�@�1@��H@�{@�G�@�1'@�+@��@���@�z�@�(�@�33@�^5@�/@�A�@��@�;d@�@��@�@���@㕁@���@߶F@�@���@���@�M�@��@�O�@�&�@��`@�I�@��@׾w@�@��y@ָR@�v�@ա�@�X@ԓu@�
=@�&�@�\)@��y@�-@͑h@� �@�x�@�/@��@��/@��@�C�@�$�@�?}@Ĭ@�A�@�  @��H@��@���@��@��@��
@��y@��+@�J@���@���@��@�(�@�+@�ȴ@��!@��\@���@�ƨ@�33@�n�@��@��@���@�=q@���@�?}@���@��D@��@��R@���@���@�X@�9X@��w@��@��@�5?@�hs@��/@��@�ƨ@�\)@�S�@�S�@�v�@��@�hs@��@��j@�z�@��m@�l�@���@���@��m@�Z@�Z@��w@�dZ@�S�@�33@�ȴ@�~�@�$�@��@���@�Q�@��P@��@���@�n�@���@�J@��@�=q@�^5@�V@�ff@�v�@�$�@�$�@�=q@�=q@�=q@���@���@��@��#@�hs@�C�@��-@��h@�`B@�G�@�V@��@��@�7L@�G�@�?}@�O�@��#@��\@�n�@�J@��@�\)@�o@�o@�M�@��7@�&�@���@�bN@��@�t�@�o@�@���@�V@��T@���@�O�@�/@��`@��@�1@�C�@��@�@��H@��@�C�@��@���@��P@�l�@��H@��\@�=q@��@���@���@��7@�?}@���@��`@��`@��/@��@��D@��@�r�@�Z@��@��m@��P@�t�@�K�@�@���@�M�@�=q@�{@�hs@��@�/@���@�j@�9X@��
@��F@��F@�ƨ@��
@�dZ@�o@��@���@���@���@��@���@���@�-@�/@��@���@�r�@��
@��@��P@���@���@��@��P@���@��@�|�@�t�@�C�@�@��@��+@�~�@�~�@��-@�X@�G�@�7L@�%@���@|��@s��@k�@aN<@W� @O��@K i@C�;@=�@8�@1@@)��@"5?@Xy@c�@��@�$@��@�*@	��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AǼjAǸRAǉ7A��A�dZA�{A��TA�x�AľwAĥ�Aġ�Aĝ�Aĕ�AčPAċDAąA�jA�Q�AþwA��A��`A���A®AuA�ffA�^5A�K�A�-A�
=A��A��mA��#A���A���A��A�=qA��wA�ffA��jA��-A�l�A���A�-A�\)A�ZA�ffA�A�\)A���A��+A�l�A� �A���A��A�ȴA��RA�l�A��^A�l�A�=qA���A�G�A��A�%A��`A���A��uA�^5A��jA�jA�ffA��A�bA�n�A��A��/A���A�(�A��mA�I�A�?}A�
=A��9A���A�^5A���A�A�ffA�bA���A�E�A��A��wA�Q�A���A�{A�`BA��/A�^5A���A���A��7A��RA�M�A�oA�O�A���A��A��A�oA~�/A{�Ay�Ax�AxQ�Aw�AuK�At��AsK�Ar�\Aqp�Ap^5AnI�Am�Al�Ak�wAjv�Ah�!AhbAg|�Af��AfjAd�yAb^5A`1A\ĜA[O�AZv�AY��AY7LAX�yAX��AW�AV��AS�AOx�ANȴAN~�AM��AJz�AH�RAHI�AF9XAC�TACG�AB�yAB��ABffAA�
AAO�A<ĜA9��A9�A7C�A4A3%A1��A/�A-�7A+�wA)dZA)�A'7LA$�A#oA"I�A!��A!��A!�FA VA�A%AO�AdZAJAz�A�A�A�A��AVAz�A�
A"�A�!A��A
ffA	7LA�^A|�A�Av�A=qA��AC�Ar�A�A��A ��@��
@���@�@�bN@��R@���@�&�@�1@��H@�{@�G�@�1'@�+@��@���@�z�@�(�@�33@�^5@�/@�A�@��@�;d@�@��@�@���@㕁@���@߶F@�@���@���@�M�@��@�O�@�&�@��`@�I�@��@׾w@�@��y@ָR@�v�@ա�@�X@ԓu@�
=@�&�@�\)@��y@�-@͑h@� �@�x�@�/@��@��/@��@�C�@�$�@�?}@Ĭ@�A�@�  @��H@��@���@��@��@��
@��y@��+@�J@���@���@��@�(�@�+@�ȴ@��!@��\@���@�ƨ@�33@�n�@��@��@���@�=q@���@�?}@���@��D@��@��R@���@���@�X@�9X@��w@��@��@�5?@�hs@��/@��@�ƨ@�\)@�S�@�S�@�v�@��@�hs@��@��j@�z�@��m@�l�@���@���@��m@�Z@�Z@��w@�dZ@�S�@�33@�ȴ@�~�@�$�@��@���@�Q�@��P@��@���@�n�@���@�J@��@�=q@�^5@�V@�ff@�v�@�$�@�$�@�=q@�=q@�=q@���@���@��@��#@�hs@�C�@��-@��h@�`B@�G�@�V@��@��@�7L@�G�@�?}@�O�@��#@��\@�n�@�J@��@�\)@�o@�o@�M�@��7@�&�@���@�bN@��@�t�@�o@�@���@�V@��T@���@�O�@�/@��`@��@�1@�C�@��@�@��H@��@�C�@��@���@��P@�l�@��H@��\@�=q@��@���@���@��7@�?}@���@��`@��`@��/@��@��D@��@�r�@�Z@��@��m@��P@�t�@�K�@�@���@�M�@�=q@�{@�hs@��@�/@���@�j@�9X@��
@��F@��F@�ƨ@��
@�dZ@�o@��@���@���@���@��@���@���@�-@�/@��@���@�r�@��
@��@��P@���@���@��@��P@���@��@�|�@�t�@�C�@�@��@��+@�~�@�~�@��-@�X@�G�@�7L@�%G�O�@|��@s��@k�@aN<@W� @O��@K i@C�;@=�@8�@1@@)��@"5?@Xy@c�@��@�$@��@�*@	��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBn�Bn�Bn�By�B�{B��B�-BÖB�B�#B�#B�#B�B�B�B�#B�yB�BJB:^BO�BS�B\)Be`Bo�Bq�Bu�B|�B�%B�=B�JB�oB�{B��B�{B�hB�1B�Bs�B~�B�B{�By�Bt�Bw�Bx�By�By�Bz�B{�B{�B|�B|�B|�B|�B|�B|�B~�B~�B~�B�B~�B�B�B� B~�B�B�B�B� B�B}�B{�Bw�Bt�Br�Bp�Bm�BiyBdZBZBL�BE�BD�B>wB6FB-B#�B�B�B��B�BŢB�wB�XB�B��B��B�BS�BG�B2-B"�B\B
��B
�;B
�RB
�oB
y�B
k�B
C�B
,B
�B
�B
uB
VB
B
B
B
B
  B	��B	�B	�B	�B	�HB	�#B	��B	��B	ȴB	ÖB	�wB	�B	��B	~�B	gmB	YB	R�B	M�B	I�B	E�B	A�B	=qB	33B	&�B	JB	%B	B��B�B�TB�HB�B��BÖB��B�wB�jB�XB�?B��B�VB�+B�Bn�Bs�Bn�Bo�BgmBcTBXBQ�BP�BN�BL�BK�BK�BJ�BH�BN�BL�BK�BM�BO�BL�BJ�BE�BD�BC�BA�B?}B<jB<jB<jB<jB;dB8RB33B33B1'B.B-B-B,B,B-B+B+B,B,B/B/B0!B+B)�B(�B'�B&�B&�B&�B&�B%�B%�B&�B&�B&�B'�B'�B(�B)�B)�B+B+B)�B+B,B-B1'B1'B7LB=qB?}B@�BA�BB�BB�BB�BC�BC�BC�BD�BD�BD�BD�BF�BE�BF�BG�BI�BK�BK�BK�BJ�BK�BQ�BQ�BQ�BQ�BT�BVBZB\)B]/B^5B_;BbNBcTBdZBe`BffBiyBl�Bn�Bp�Bp�Bq�Bs�Bw�B|�B}�B}�B}�B�7B�JB�\B�hB��B��B��B��B��B��B��B��B�B�3B�LB�RB�^B�qB�wB�}B��BBÖBĜBŢBȴB��B��B��B�B�
B�B�#B�)B�;B�B�B��B��B	B	%B	1B	PB	bB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	(�B	2-B	5?B	9XB	;dB	<jB	=qB	A�B	I�B	M�B	O�B	P�B	R�B	W
B	YB	\)B	\)B	\)B	W
B	R�B	W
B	ZB	[#B	\)B	]/B	`BB	bNB	bNB	dZB	ffB	k�B	o�B	t�B	t�B	s�B	s�B	r�B	s�B	t�B	v�B	v�B	x�B	y�B	z�B	~�B	�B	�B	�B	�1B	�DB	�JB	�VB	�\B	�bB	�bB	�hB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�9B	�?B	�FB	�LB	�LB	�LB	�RB	�XB	�jB	�}B	��B	��B	��B	B	B	B	B	B	B	ÖB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�
B	�
B	�
B	�B	�)B	�/B	�5B	�;B	�;B	�;B	�;B	�BB	�ZB	�fB	�B	�B	�B	�B	�B	�B	��B
 �B

�B
~B
,�B
3�B
;dB
A�B
GB
H1B
PbB
WYB
^�B
eB
hsB
j�B
n/B
r�B
w2B
y�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BfTBfTBfTBq�B�5B��B��B�MB��B��B��B��B��B��B��B��B�.B�RB�B2BG�BK�BS�B]BgKBiWBmoBt�B}�B��B��B�B�%B�+B�%B�B�B{�BkcBv�Bx�Bs�Bq�BljBo~Bp�Bq�Bq�Br�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bt�Bv�Bv�Bv�Bx�Bv�Bx�Bx�Bw�Bv�Bx�Bx�Bx�Bw�By�Bu�Bs�Bo�BlmBjaBhUBeBBa+B\BQ�BD�B=VB<PB6,B-�B$�B�B]B?B�B��B�`B�5B�B��B�yB�BBz�BK�B?uB)�B�B'B
�B
�
B
�#B
�CB
q�B
c\B
;pB
#�B
�B
jB
RB
4B	��B	��B	��B	��B	��B	��B	�B	�B	�fB	�*B	�B	��B	êB	��B	�zB	�[B	� B	��B	v�B	_XB	QB	J�B	E�B	A�B	=�B	9vB	5_B	+!B	�B	<B�B�B��B�B�IB�=B��B¸B��B�zB�oB�bB�PB�7B��B�QB'BzBf�Bk�Bf�Bg�B_lB[TBPBI�BH�BF�BD�BC�BC�BB�B@�BF�BD�BC�BE�BG�BD�BB�B=�B<�B;�B9�B7�B4oB4oB4oB4oB3iB0XB+9B+9B).B&B%B%B$B$B%B#
B#
B$B$B'#B'#B()B#
B"B �B�B�B�B�B�B�B�B�B�B�B�B�B �B"B"B#B#B"B#B$B%B)0B)0B/UB5zB7�B8�B9�B:�B:�B:�B;�B;�B;�B<�B<�B<�B<�B>�B=�B>�B?�BA�BC�BC�BC�BB�BC�BI�BI�BI�BI�BMBNBR%BT1BU7BV=BWCBZVB[\B\bB]hB^nBa�Bd�Bf�Bh�Bh�Bi�Bk�Bo�Bt�Bu�Bu�Bu�B�=B�PB�bB�nB��B��B��B��B��B��B��B�B�B�7B�PB�VB�bB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B� B�&B�+B�=B�B�B��B��B�B�%B	 1B	PB	aB	
nB	tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	*+B	-<B	1UB	3aB	4gB	5nB	9�B	A�B	E�B	G�B	H�B	J�B	OB	QB	T$B	T$B	T$B	OB	J�B	OB	RB	SB	T$B	U*B	X=B	ZIB	ZIB	\UB	^aB	cB	g�B	l�B	l�B	k�B	k�B	j�B	k�B	l�B	n�B	n�B	p�B	q�B	r�B	v�B	{B	{B	}B	�*B	�=B	�CB	�OB	�UB	�[B	�[B	�aB	�hB	�hB	�nB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�*B	�0B	�0B	�6B	�=B	�CB	�CB	�CB	�IB	�OB	�`B	�sB	�yB	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�*B	�0B	�0B	�0B	�0B	�7B	�OB	�ZB	�B	�B	�B	�B	�G�O�B	��B	��B
�B
pB
$�B
+�B
3UB
9�B
?B
@"B
HSB
OJB
V�B
]B
`cB
b�B
fB
j�B
o"B
q{B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144152022020411441520220204114415  AO  ARCAADJP                                                                    20200619170905    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170905  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170905  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114415  IP                  G�O�G�O�G�O�                