CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-08-01T15:36:39Z creation;2019-08-01T15:36:43Z conversion to V3.1;2022-11-21T05:28:34Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  M�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Up   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Wh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  i   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  k    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  t�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  |�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190801153639  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_181                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�ћ%�	�1   @�ќhK� @<mO�;dZ�dn�1���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B33BffB��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�D{f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�oA��A��A��A��A��A��A��A��A��A�bA�
=A�A�JA�%A��#A�ffA���Aɧ�A�bA�Q�AƲ-A���A���A�K�A�M�A�jA�I�A���A�7LA��PA�A�  A���A��RA�
=A�v�A���A��A���A��A�
=A�hsA��mA�  A��9A�;dA���A��A��DA�hsA��A���A�=qA��`A��9A�r�A��FA�G�A�ZA��A�VA�A��RA�=qA��-A�p�A�&�A��A�l�A���A�A���A��A���A�\)A��!A��DA���A�n�A�  A��PA��TA�1A�jA��
A���A�hsA�33A��
A���A�A�A���A�?}A���A~�A{��A{&�Ay|�Ax�Aw�hAv��Av9XAut�Au%As��Aq�AqVAp�9Ao�7An��Al��Ak�;Ak&�Aj  Ai/Ag|�Ad�yAcdZAa�-A_��A_A_%A^�/A]S�A[S�AZbNAY?}AV �AS�AQ��AQ&�AP�yAO�FAL�RAL1AK33AJ�HAJ(�AH~�AGXAF�AEVAC�AA�-A@(�A?/A>�A>{A=�#A=%A<JA:��A9�^A9dZA9%A8�\A8$�A6��A5?}A4�`A4=qA3�-A2��A1p�A01A/dZA/%A.I�A-VA,I�A,A+�#A+A+7LA*E�A)VA(Q�A(A'�mA'�A&n�A&$�A%|�A%oA$�9A$E�A"�A!�A �HA �jA ��A A�A��A�A(�A�AoA$�A��A+A9XA��AhsA�mA�A�uAK�A��A{A�wAXA�HA��A;dA��AJA��A+A
��A
�A
E�A
�A	�A	�^A	7LA��A�9A�uAM�A��A�AȴA�A
=A��A5?A��Al�A�A��A�AƨA �A @�\)@���@�+@���@��@���@�ff@�{@�Ĝ@�!@�7@��@��@�@�V@�l�@�@�j@��;@���@�+@�X@㕁@�@�hs@���@��@�I�@��@��@�r�@� �@ۍP@ڇ+@��#@�V@�1'@���@�@�1'@�1'@��;@���@�M�@�J@�hs@ύP@Χ�@̬@�o@��@�@�X@ȋD@�l�@��@���@ă@�z�@�bN@���@�l�@�S�@�o@�@��`@�;d@��R@�{@�G�@��@��@��@�ff@�V@�J@��@��@�+@���@���@�ff@���@���@�E�@�hs@��j@�b@�l�@��#@���@���@�@�5?@�@��@���@�  @��F@�o@��+@�ff@��#@�X@���@��j@��j@��9@��u@�bN@��@���@�S�@��@�v�@���@���@�r�@�1@�C�@�V@��^@���@��@���@�
=@�@�`B@�/@���@���@�ff@�@��T@���@��-@���@�x�@�?}@�%@��`@��9@���@��D@�Q�@�b@��@��y@�5?@��7@���@�Z@�1'@�b@��m@�o@��+@�E�@�@���@�hs@�7L@�%@���@���@�r�@�A�@�  @��@�l�@�o@���@�&�@���@��m@���@�\)@��!@�5?@�{@���@��@�%@��/@���@�Ĝ@��u@�bN@�(�@��P@�33@��@��@���@��h@��h@��7@�X@�G�@�?}@�/@�&�@��@���@���@���@�Z@�b@�@K�@~��@~�@~��@~ff@~$�@}�h@}�@}�h@|�j@|j@|(�@|1@{S�@z^5@z-@zJ@y�@y�7@x�u@xA�@w�;@wK�@v�R@u�@sƨ@sdZ@s"�@r��@r��@r�!@r�\@r�\@rn�@rM�@rJ@q�7@q7L@p�`@p�u@pQ�@p1'@p  @ol�@n�+@nV@n{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�oA��A��A��A��A��A��A��A��A��A�bA�
=A�A�JA�%A��#A�ffA���Aɧ�A�bA�Q�AƲ-A���A���A�K�A�M�A�jA�I�A���A�7LA��PA�A�  A���A��RA�
=A�v�A���A��A���A��A�
=A�hsA��mA�  A��9A�;dA���A��A��DA�hsA��A���A�=qA��`A��9A�r�A��FA�G�A�ZA��A�VA�A��RA�=qA��-A�p�A�&�A��A�l�A���A�A���A��A���A�\)A��!A��DA���A�n�A�  A��PA��TA�1A�jA��
A���A�hsA�33A��
A���A�A�A���A�?}A���A~�A{��A{&�Ay|�Ax�Aw�hAv��Av9XAut�Au%As��Aq�AqVAp�9Ao�7An��Al��Ak�;Ak&�Aj  Ai/Ag|�Ad�yAcdZAa�-A_��A_A_%A^�/A]S�A[S�AZbNAY?}AV �AS�AQ��AQ&�AP�yAO�FAL�RAL1AK33AJ�HAJ(�AH~�AGXAF�AEVAC�AA�-A@(�A?/A>�A>{A=�#A=%A<JA:��A9�^A9dZA9%A8�\A8$�A6��A5?}A4�`A4=qA3�-A2��A1p�A01A/dZA/%A.I�A-VA,I�A,A+�#A+A+7LA*E�A)VA(Q�A(A'�mA'�A&n�A&$�A%|�A%oA$�9A$E�A"�A!�A �HA �jA ��A A�A��A�A(�A�AoA$�A��A+A9XA��AhsA�mA�A�uAK�A��A{A�wAXA�HA��A;dA��AJA��A+A
��A
�A
E�A
�A	�A	�^A	7LA��A�9A�uAM�A��A�AȴA�A
=A��A5?A��Al�A�A��A�AƨA �A @�\)@���@�+@���@��@���@�ff@�{@�Ĝ@�!@�7@��@��@�@�V@�l�@�@�j@��;@���@�+@�X@㕁@�@�hs@���@��@�I�@��@��@�r�@� �@ۍP@ڇ+@��#@�V@�1'@���@�@�1'@�1'@��;@���@�M�@�J@�hs@ύP@Χ�@̬@�o@��@�@�X@ȋD@�l�@��@���@ă@�z�@�bN@���@�l�@�S�@�o@�@��`@�;d@��R@�{@�G�@��@��@��@�ff@�V@�J@��@��@�+@���@���@�ff@���@���@�E�@�hs@��j@�b@�l�@��#@���@���@�@�5?@�@��@���@�  @��F@�o@��+@�ff@��#@�X@���@��j@��j@��9@��u@�bN@��@���@�S�@��@�v�@���@���@�r�@�1@�C�@�V@��^@���@��@���@�
=@�@�`B@�/@���@���@�ff@�@��T@���@��-@���@�x�@�?}@�%@��`@��9@���@��D@�Q�@�b@��@��y@�5?@��7@���@�Z@�1'@�b@��m@�o@��+@�E�@�@���@�hs@�7L@�%@���@���@�r�@�A�@�  @��@�l�@�o@���@�&�@���@��m@���@�\)@��!@�5?@�{@���@��@�%@��/@���@�Ĝ@��u@�bN@�(�@��P@�33@��@��@���@��h@��h@��7@�X@�G�@�?}@�/@�&�@��@���@���@���@�Z@�b@�@K�@~��@~�@~��@~ff@~$�@}�h@}�@}�h@|�j@|j@|(�@|1@{S�@z^5@z-@zJ@y�@y�7@x�u@xA�@w�;@wK�@v�R@u�@sƨ@sdZ@s"�@r��@r��@r�!@r�\@r�\@rn�@rM�@rJ@q�7@q7L@p�`@p�u@pQ�@p1'@p  @ol�@n�+@nV@n{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB��B�!B�?B�LB��B�B�ZB�sB�B�yB�B�B�sB�HB�#B��B��BɺB�wB�?B�B��B�PBw�Bn�B\)BR�BN�BJ�BG�B@�B9XB0!B&�B �B�B�B�BuBDB��B�B�B�B�sB�ZB�5B�
B��B��B�9B��B��B�VB~�Bs�Bn�BhsB]/BJ�BA�B8RB/B�B\BB
��B
�B
�;B
��B
��B
ɺB
�}B
�?B
�B
��B
��B
�1B
p�B
k�B
_;B
YB
Q�B
J�B
G�B
A�B
<jB
5?B
,B
&�B
#�B
�B
�B
JB
B
  B	��B	�B	�mB	�
B	��B	B	�FB	�-B	�-B	�B	��B	��B	�uB	�=B	s�B	]/B	R�B	VB	T�B	N�B	?}B	?}B	=qB	<jB	5?B	&�B	�B	oB	%B��B�B�`B�TB�`B�`B�B�B�`B�;B�B�
B�
B��B��BƨB�qB�jB�XB�FB�'B�B��B��B��B��B��B��B��B��B��B�{B�bB�PB�=B�1B�+B�%B�B�B� B~�B}�B}�B� B|�Bz�By�By�Bx�Bu�Bs�Bp�Bl�BhsBe`BdZBbNB`BB^5B[#BT�BQ�BO�BN�BM�BM�BL�BK�BI�BF�BD�BC�BD�BC�BA�B<jB:^B:^B:^B9XB8RB8RB8RB8RB8RB8RB7LB:^B:^B8RB6FB5?B5?B5?B49B33B33B1'B0!B.B,B)�B(�B&�B&�B$�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B#�B&�B'�B'�B)�B.B.B0!B/B/B/B/B/B0!B1'B33B1'B1'B1'B1'B1'B33B5?B5?B8RB:^B<jB<jB>wB?}B@�BB�BF�BI�BK�BJ�BJ�BJ�BJ�BJ�BK�BL�BM�BM�BN�BO�BP�BQ�BS�BVBZB\)B^5B`BBbNBcTBe`Be`BgmBhsBjBk�Bk�Bm�Bn�Bo�Bp�Bp�Bp�Bp�Bq�Br�Bs�Bt�Bu�Bv�Bw�Bz�B|�B~�B�B�%B�7B�JB�PB�hB�{B��B��B��B��B��B�B�B�B�B�!B�!B�'B�-B�3B�3B�?B�?B�?B�LB�RB�^B�wBÖBǮB��B��B��B��B��B�B�B�#B�/B�BB�HB�NB�ZB�`B�fB�sB�yB�B�B�B�B��B��B	B	B	+B		7B	PB	bB	hB	oB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	&�B	0!B	49B	5?B	5?B	5?B	7LB	8RB	9XB	9XB	:^B	:^B	;dB	=qB	>wB	A�B	D�B	F�B	H�B	I�B	I�B	J�B	K�B	L�B	O�B	O�B	O�B	R�B	T�B	VB	W
B	YB	_;B	`BB	`BB	aHB	cTB	gmB	hsB	iyB	l�B	o�B	u�B	z�B	{�B	|�B	~�B	~�B	~�B	� B	� B	� B	� B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�JB	�bB	�hB	�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�-B��B�IB�B�B��B�B��BܬB�B��B�B�OB�B�5B�B��B�B�B�B�jB�uB�XB�iB��B��By�Bq�B]�BS�BPBL0BIBB'B;�B2B(sB!�BxB7B�B�B<B��B��B�B�WB�DB�zB�VB��B�&B�B��B��B��B�4B�iBt�Bo�BjeB_pBLdBB�B9�B2BxBNB�B
�fB
�B
�BB
��B
�4B
ˬB
� B
��B
�CB
��B
��B
�B
q�B
mCB
`BB
Z�B
SB
K�B
H�B
B�B
>(B
6�B
-B
'�B
%`B
!B
B
�B
YB
�B	�JB	�B	�KB	�1B	��B	ĜB	��B	�|B	��B	�AB	�$B	�=B	��B	�B	wB	^�B	S�B	V�B	V�B	Q�B	@�B	@�B	>(B	=�B	72B	(�B	pB	FB	�B��B�iB�B�&B�LB�B�B��B��B�vBخBרB��B��BѝB�fB�(B�VB�DB��B�B��B��B��B��B�NB��B�B��B��B��B��B��B�<B��B��B�1B��B��B��B��B�B~�B�B��B}�B{0Bz^Bz�By�Bv�BuZBr�Bm�Bi�Bf2Be,Bc�B`�B_;B\�BVBR�BQ�BO�BN�BN�BM�BL�BKBG�BEmBD�BESBEBC�B="B:�B:�B:�B9�B9	B8�B8�B8�B8�B9	B8lB;0B;�B9rB6�B5�B5�B5�B4�B3�B3�B2GB1[B/OB,�B+kB*�B'�B'�B%�B$�B#nB!�B!B�B �B �BxBdB�B�ByB
B?B
BB�BsB�B�B�B
B�B�BCB/BIBdBjB�B�B�B �B!�B$&B'mB(�B(sB*B.�B/OB1B0�B0!B/�B/�B/�B0�B2B4B1�B1vB1vB1[B1�B3�B5tB5�B9$B;JB=qB<�B?B@BA BCGBG_BJ#BK�BKDBKxBK�BK^BK)BLBMPBN�BN�BO�BP�BQ�BR�BT�BW?BZ�B]B^�B`�Bb�Bc�Be�BfBg�Bh�BkBk�BlBm�Bn�Bo�Bp�Bp�Bp�Bp�BrBsBtBu%BvFBw�BxlB{dB}qB�B��B��B��B��B�B� B�2B�1B��B�IB��B��B�WB�/B�5B�5B�UB�oB�vB�aB��B�hB�tB��B��B��B��B��B�B�3B�1B�B�B�B�.BѝBևB�KB�qB�~B��B�|B�B�B�B�B��B��B��B��B�B�B�lB�qB	�B	mB	zB		�B	�B	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	OB	"B	$ZB	'�B	0UB	49B	5ZB	5tB	5�B	7fB	8�B	9rB	9rB	:xB	:�B	;�B	=�B	>�B	A�B	D�B	F�B	H�B	I�B	I�B	J�B	K�B	MB	O�B	PB	P.B	SB	UB	VSB	WYB	YeB	_VB	`vB	`vB	a|B	c�B	g�B	h�B	i�B	l�B	pUB	v+B	{B	|B	}"B	~�B	B	.B	� B	�4B	�4B	�OB	�;B	�GB	�SB	�YB	�_B	�fB	�fB	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<>�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.05(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201908120031392019081200313920190812003139202211182139512022111821395120221118213951201908130016292019081300162920190813001629  JA  ARFMdecpA19c                                                                20190802003638  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190801153639  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190801153641  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190801153641  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190801153642  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190801153642  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190801153642  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190801153642  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190801153643                      G�O�G�O�G�O�                JA  ARUP                                                                        20190801155515                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190801153143  CV  JULD            G�O�G�O�Fƌ�                JM  ARCAJMQC2.0                                                                 20190811153139  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190811153139  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190812151629  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123951  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                