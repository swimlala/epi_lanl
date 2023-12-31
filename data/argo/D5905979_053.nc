CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:06Z creation      
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  yp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20200619170906  20220204114416  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               5A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؠ]q���1   @ؠ]��>@5ѩ��l��c�n��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    5A   B   B   @�  @�33A��A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/fD/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�=D�qD�IHD��D�ǮD�  D�W
D���D�њD�$)D�\{D��qD�� D��D�Z�Dډ�D࿮D��D�S3D�q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@���A ��A33A=��A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3CuٙCw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D3D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D3D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$�3D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.�3D/3D/|�D/�gD0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD�gDE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��DtvgDy�
D��D�G�D���D��D�fD�UpD��)D�� D�"�D�Z�D���D��fD�\D�YGDڈRD�D�RD�Q�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1A��A��A�VA�
=A�oA�(�A�/A�/A�/A�/A�1'A�33A�5?A�5?A�7LA�5?A�1'A�/A�
=A�bNA�S�A�S�A��A��`A���A�ƨA�ƨAº^A£�A+A�~�A�Q�A�"�A���A��RA���A���A���A��A�`BA���A���A��A�E�A��wA�$�A�5?A���A�p�A�1A���A� �A�dZA�r�A��A�9XA�+A�S�A�A�x�A�33A���A��uA�jA��+A��+A�=qA��A�r�A��;A���A�7LA���A��A�M�A�`BA�K�A�bA�1'A���A���A���A�$�A��A��A�`BA��A�
=A�ZA�ZA�5?A��!A�%A���A��yA�G�A���A��7A��A�/A���A�=qA�VA�oA�A��A�t�A��A�v�A�~�A�%A}�Az1'Aw&�Au/Ar1'Ao�#AnZAl��Ak7LAex�A`�RA_�A_S�A^A]C�A\ȴA\5?A[�AZĜAY�AX��AVM�AT  AP=qAK�#AJ1AGG�AC�
AA�A@�/A?��A?x�A>��A=�A<��A;�PA8�HA6bA3`BA/�A.�A,��A(�DA'
=A%33A!��A �yA z�A   A�A�FA��A�yA�9A�+A=qAoA�uAE�AbA �A5?AI�AE�Ax�A�AĜA"�A�A��AVA �A��A��A�;AƨA��A
�jA
A�A�AZAS�A��A7LA%A%A�AbAl�A��A�A�^A �@���@��T@�@��-@�I�@��@�dZ@��R@���@��D@���@��@�Z@��@�S�@�7@�F@�@@���@�\)@���@畁@�n�@�`B@�D@�w@���@�X@��@��@���@��/@��/@�bN@��@��@�(�@�{@�p�@�7L@�Ĝ@�bN@��@�C�@�ff@��/@�C�@���@җ�@Ѳ-@�&�@�&�@��@Гu@�9X@υ@Η�@ˮ@ɡ�@�z�@ǅ@�J@ź^@�?}@ě�@�Z@�b@�ƨ@��@°!@�v�@�ff@�-@�@���@�x�@�z�@�S�@��@�-@��7@��@��;@�@�$�@�V@�t�@�hs@�(�@�K�@��@��@��u@�I�@�S�@��@��@�ȴ@���@��@��j@��D@�j@�I�@�9X@��@���@��!@�E�@�-@�$�@�@��h@�?}@��`@���@��@�S�@�$�@���@��y@���@�~�@�n�@�ff@�E�@��#@�%@�Ĝ@��@��@�9X@�33@���@���@���@��\@��+@�=q@���@��
@�S�@��@���@���@�n�@�-@�{@�J@�@�@���@��@���@���@�p�@��/@�Z@�1'@� �@�b@�|�@��y@��\@���@�G�@�V@��@���@�V@�V@�%@���@��`@��`@�Ĝ@���@�r�@�I�@�A�@�A�@�Q�@�9X@� �@�1@��@��@��@�-@���@�X@�O�@���@��9@��@��@��@��D@� �@��P@�\)@�33@�o@��@���@��R@��!@��!@��!@��!@��!@�v�@�@���@�/@���@��@�bN@�1'@�Q�@�bN@�j@�j@�bN@�Z@�j@�bN@�1'@��
@��F@���@�C�@�;d@�;d@�"�@��@�ȴ@��R@��!@��R@��R@��!@���@���@���@�~�@�v�@�^5@�-@�J@��@��@��@���@�@��^@���@��h@�x�@��j@��D@�r�@��m@���@��F@���@��P@��@��@�dZ@�33@�"�@��@�o@���@��!@��+@�~�@�v�@�V@�5?@�-@�;@zh
@p��@d�I@^��@Yx�@V^5@Im]@B@�@;�4@5��@0:�@&��@��@U�@]d@��@_@:*@s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A�1A��A��A�VA�
=A�oA�(�A�/A�/A�/A�/A�1'A�33A�5?A�5?A�7LA�5?A�1'A�/A�
=A�bNA�S�A�S�A��A��`A���A�ƨA�ƨAº^A£�A+A�~�A�Q�A�"�A���A��RA���A���A���A��A�`BA���A���A��A�E�A��wA�$�A�5?A���A�p�A�1A���A� �A�dZA�r�A��A�9XA�+A�S�A�A�x�A�33A���A��uA�jA��+A��+A�=qA��A�r�A��;A���A�7LA���A��A�M�A�`BA�K�A�bA�1'A���A���A���A�$�A��A��A�`BA��A�
=A�ZA�ZA�5?A��!A�%A���A��yA�G�A���A��7A��A�/A���A�=qA�VA�oA�A��A�t�A��A�v�A�~�A�%A}�Az1'Aw&�Au/Ar1'Ao�#AnZAl��Ak7LAex�A`�RA_�A_S�A^A]C�A\ȴA\5?A[�AZĜAY�AX��AVM�AT  AP=qAK�#AJ1AGG�AC�
AA�A@�/A?��A?x�A>��A=�A<��A;�PA8�HA6bA3`BA/�A.�A,��A(�DA'
=A%33A!��A �yA z�A   A�A�FA��A�yA�9A�+A=qAoA�uAE�AbA �A5?AI�AE�Ax�A�AĜA"�A�A��AVA �A��A��A�;AƨA��A
�jA
A�A�AZAS�A��A7LA%A%A�AbAl�A��A�A�^A �@���@��T@�@��-@�I�@��@�dZ@��R@���@��D@���@��@�Z@��@�S�@�7@�F@�@@���@�\)@���@畁@�n�@�`B@�D@�w@���@�X@��@��@���@��/@��/@�bN@��@��@�(�@�{@�p�@�7L@�Ĝ@�bN@��@�C�@�ff@��/@�C�@���@җ�@Ѳ-@�&�@�&�@��@Гu@�9X@υ@Η�@ˮ@ɡ�@�z�@ǅ@�J@ź^@�?}@ě�@�Z@�b@�ƨ@��@°!@�v�@�ff@�-@�@���@�x�@�z�@�S�@��@�-@��7@��@��;@�@�$�@�V@�t�@�hs@�(�@�K�@��@��@��u@�I�@�S�@��@��@�ȴ@���@��@��j@��D@�j@�I�@�9X@��@���@��!@�E�@�-@�$�@�@��h@�?}@��`@���@��@�S�@�$�@���@��y@���@�~�@�n�@�ff@�E�@��#@�%@�Ĝ@��@��@�9X@�33@���@���@���@��\@��+@�=q@���@��
@�S�@��@���@���@�n�@�-@�{@�J@�@�@���@��@���@���@�p�@��/@�Z@�1'@� �@�b@�|�@��y@��\@���@�G�@�V@��@���@�V@�V@�%@���@��`@��`@�Ĝ@���@�r�@�I�@�A�@�A�@�Q�@�9X@� �@�1@��@��@��@�-@���@�X@�O�@���@��9@��@��@��@��D@� �@��P@�\)@�33@�o@��@���@��R@��!@��!@��!@��!@��!@�v�@�@���@�/@���@��@�bN@�1'@�Q�@�bN@�j@�j@�bN@�Z@�j@�bN@�1'@��
@��F@���@�C�@�;d@�;d@�"�@��@�ȴ@��R@��!@��R@��R@��!@���@���@���@�~�@�v�@�^5@�-@�J@��@��@��@���@�@��^@���@��h@�x�@��j@��D@�r�@��m@���@��F@���@��P@��@��@�dZ@�33@�"�@��@�o@���@��!@��+@�~�@�v�@�V@�5?G�O�@�;@zh
@p��@d�I@^��@Yx�@V^5@Im]@B@�@;�4@5��@0:�@&��@��@U�@]d@��@_@:*@s1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�1B�+B�+B�1B�1B�1B�+B�+B�+B�+B�+B�+B�+B�+B�+B�+B�+B�=B�VB��B�;B�BPB�B�B�B�B�B�B!�B%�B33BH�B\)Bx�B�DB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�hB�VB�JB��B��B��B��B��B��B�uB�hB�DB}�B_;BI�B@�B0!B1'B0!B-B%�B"�B �B �B�B�B�B1'BG�B@�B(�B"�B�BJB��B�B�`B��B�-B�B��B��B�{B�JB�1B�Bt�Br�BjB_;BD�BA�B<jB0!B�BbBB
��B
��B
�?B
��B
cTB
G�B
33B
�B
bB	��B	�mB	�)B	��B	��B	��B	x�B	p�B	l�B	aHB	ZB	VB	P�B	L�B	F�B	A�B	I�B	P�B	@�B	49B	oB	PB	B�B�fB�NB�B�B�
B��B��BǮB�}B�-B��B��B�{B�hB�=B�B�B|�Bv�Bu�Bs�Bq�Bp�Bp�Bk�BjBiyBgmBcTBdZBcTBdZBe`BffBgmBiyBiyBcTBhsBffB_;B\)B\)B\)BYBYBYBXBW
BVBS�BR�BR�BXBZB\)B[#BZBZB\)BYBW
BXBW
BXBVBVBYBZB\)BZBZBZBYB\)B\)B`BB_;B_;B_;B`BBaHBaHBbNB`BB`BB_;B_;BaHBcTBdZBbNBbNBbNBcTBdZBffBhsBhsBjBm�Bm�Bo�Bs�Bq�Bq�Br�Bq�Br�Bq�Bs�Bs�Bs�Bs�Br�Bs�Bs�Bs�Bs�Bt�Bu�Bv�Bx�B�B� B�B�B�1B�7B�DB�JB�JB�JB�JB�PB�VB�VB�\B�bB�hB�hB�oB�uB�{B��B��B��B��B��B��B��B�B�3B�qBĜBǮB��B��B��B�B�#B�/B�/B�/B�fB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	%B	DB	hB	�B	�B	�B	�B	�B	�B	 �B	$�B	&�B	'�B	'�B	,B	5?B	;dB	>wB	>wB	?}B	?}B	A�B	O�B	W
B	ZB	[#B	\)B	]/B	_;B	`BB	aHB	aHB	aHB	aHB	aHB	bNB	cTB	dZB	e`B	jB	n�B	n�B	n�B	n�B	r�B	t�B	u�B	z�B	}�B	�B	�B	�1B	�DB	�PB	�PB	�VB	�\B	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�9B	�9B	�LB	�FB	�LB	�FB	�FB	�FB	�RB	�^B	�dB	�jB	�qB	�wB	�wB	�}B	�}B	��B	��B	��B	�}B	��B	B	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�;B	�BB	�BB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B
B
PB
WB
�B
&fB
*0B
8�B
@iB
F�B
L~B
R�B
Z�B
d�B
fLB
h�B
kQB
m�B
k�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B�B~�B~�B�B�B�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B�B�&B��B�B�hBBIB[BhBnBtBzB�B�B*�B@yBS�Bp�B�B�0B�5B�;B�GB�MB�fB�`B�ZB�fB�`B�`B�TB�HB�BB�)B�B�B�[B�CB�CB�CB�\B�CB�8B�+B�Bu�BWBA�B8LB'�B(�B'�B$�B�B�B�B�B~BrBfB(�B?xB8MB �B�B�BB��B�nB�1BŦB�B��B��B�jB�RB�"B�	By�Bl�Bj�BbYBWB<yB9fB4HB'�BmBCB
� B
�B
ĳB
�&B
��B
[AB
?�B
+$B
�B
UB	��B	�cB	� B	��B	��B	��B	p�B	h�B	d�B	YHB	RB	NB	H�B	D�B	>�B	9�B	A�B	H�B	8�B	,=B	
vB	WB�B�B�pB�YB�"B�B�B��B��B��B��B�<B�B��B��B�zB�PByB{%BuBn�Bm�Bk�Bi�Bh�Bh�Bc�Bb�Ba�B_�B[jB\pB[jB\pB]vB^|B_�Ba�Ba�B[kB`�B^}BWSBTABTABTABQ0BQ0BQ0BP)BO#BNBLBKBKBP)BR6BTBBS<BR6BR6BTBBQ1BO$BP*BO$BP*BNBNBQ1BR7BTCBR7BR7BR7BQ1BTCBTCBX\BWUBWUBWUBX\BYbBYbBZhBX]BX]BWVBWVBYcB[oB\uBZiBZiBZiB[oB\uB^�B`�B`�Bb�Be�Be�Bg�Bk�Bi�Bi�Bj�Bi�Bj�Bi�Bk�Bk�Bk�Bk�Bj�Bk�Bk�Bk�Bk�Bl�Bm�Bn�Bp�Bz'BxB{-B|4B�LB�RB�^B�dB�dB�dB�dB�jB�pB�pB�vB�|B��B��B��B��B��B��B��B��B��B��B��B�B�!B�LB��B��B��B�B�B�B�B�:B�FB�FB�FB�}B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B�B�B�.B�:B	YB		}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	 B	$B	-RB	3vB	6�B	6�B	7�B	7�B	9�B	G�B	OB	R.B	S4B	T:B	U@B	WLB	XSB	YXB	YXB	YXB	YXB	YXB	Z^B	[dB	\jB	]pB	b�B	f�B	f�B	f�B	f�B	j�B	l�B	m�B	r�B	vB	zB	}.B	�?B	�RB	�^B	�^B	�dB	�jB	�jB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�EB	�EB	�XB	�RB	�XB	�RB	�RB	�RB	�^B	�jB	�pB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�'B	�-B	�-B	�-B	�-B	�3B	�3B	�EB	�LB	�LB	�^B	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�pB	�pB	�pB	�wB	�wB	�B	�B	�B	�B	�B	�G�O�B	�B	�B
XB
_B
�B
mB
"7B
0�B
8oB
>�B
D�B
J�B
R�B
\�B
^QB
`�B
cVB
e�B
c�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.05 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144162022020411441620220204114416  AO  ARCAADJP                                                                    20200619170906    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170906  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170906  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                