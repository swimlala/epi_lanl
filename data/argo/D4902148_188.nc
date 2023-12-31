CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-10T15:37:08Z creation;2019-10-10T15:37:13Z conversion to V3.1;2022-11-21T05:28:13Z update;     
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
resolution        =���   axis      Z          :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   B   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       D   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       N   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   V$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       X(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       b4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   j<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       l@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   tH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       vL   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       ~T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191010153708  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_188                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��8�w 1   @���-� @;����%��dbě��T1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0��B8��B?�
BG�
BP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C(�C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D
=D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~�=D�D��D��D�A�D��D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�?}A�E�A�I�A�I�A�I�A�I�A�G�A�G�A�A�A�5?A�&�A�oA��AָRAփA��AծAՕ�A�p�A�l�A�K�A���Aԥ�A�+AʼjA�r�A���A�1'A�$�A���A��A��HA�E�A���A��DA���A��DA��A���A�1'A�t�A���A�
=A�1'A���A��9A��A�5?A�ȴA�jA��^A�hsA��jA�;dA��-A�"�A���A�{A��DA�?}A��A�-A��\A��!A�ĜA���A�&�A��RA��+A�~�A�VA���A��uA�&�A���A��A� �A��A��A���A� �A�jA�-A�7LA�ĜA�A���A� �A��A�G�A�|�A���A���A�G�A~  A|�`A|1'A{��Az��AzM�Ax�jAw33Avz�At�jAs�Ar�uAq7LApffAo�Ao33Am;dAk��Aj��Ai��Ag�#Ag�Ag%Af�`Af�DAfjAe�FAe�Ad��AdM�Ac�wAcdZAbM�Aal�A_
=A\JAZ�HAY�TAYK�AX^5AW�AU��ASx�AS�AR��AR�DARJAP�DAN�yAM��AL�DAKl�AJ��AI�mAH��AHJAG��AF��AE�hAD�9ADn�AD �AC�PAB�AB��AB�A@��A?��A>�A>(�A<jA:�yA:�A9��A8jA8  A7�A6��A5l�A4�uA3%A2�DA2�A1+A0��A0�A/�-A/A.A�A-hsA,jA*�A)ƨA)�A(M�A'�A'O�A&��A%�hA%;dA$  A#C�A"��A"jA"^5A"E�A"-A"JA!`BA ��A r�A�Ap�A��AQ�A\)Av�A{Ap�A/A�jA�+A5?A��AVAAO�A�`AjAl�A+A��A{AG�A �A&�A�uAA9XA%A
ffA	�A	&�A��AVA��A��A��AA�A\)Ar�A�/A�!A{Ax�A �@�@���@��@�A�@�9X@�  @��m@��F@�@�@�ȴ@���@�(�@��@��@�G�@� �@�!@��@��`@�D@�@�v�@�  @�!@��D@ޟ�@�7L@�l�@��@�@׾w@֏\@�J@���@�G�@�j@�ȴ@��/@�@̛�@�1'@�ƨ@�"�@�hs@Ȭ@�  @���@þw@§�@�7L@�9X@�\)@�~�@��@�|�@�ȴ@�=q@�J@��-@��u@�(�@��P@��@��@�/@�(�@��@�=q@��#@��@�+@�$�@��#@��^@���@��7@��@�x�@�?}@�  @�S�@�+@�ȴ@�ff@�@���@�p�@�V@�Ĝ@���@�(�@�|�@�o@�~�@��#@�O�@��j@��/@��@��D@�Z@��
@��y@��\@��@�hs@�%@���@���@�j@���@���@���@��7@��@��@�+@���@��9@�1'@�C�@��+@��^@���@���@�`B@���@��@�z�@�Q�@��
@�33@��@�n�@�$�@���@���@��^@��-@�p�@��@���@���@�A�@��m@�o@��H@���@��+@�^5@�$�@���@���@�hs@�O�@�%@�z�@�(�@��
@���@�dZ@�;d@��@�@��y@�ȴ@�n�@�-@���@�p�@�V@���@�1'@� �@�1@���@���@��@��+@�^5@�V@��@�X@���@��/@��9@��@�dZ@�t�@�\)@���@�v�@�E�@��@��7@�x�@�hs@��@���@��9@��D@�r�@��/@���@��u@�b@�@l�@+@~�R@}�@~E�@~{@|��@|�j@{�
@z�\@z�@y��@yG�@xĜ@x�u@x �@w|�@v��@v��@vff@v@u�@t9X@sdZ@r��@r��@r��@s@r~�@r�@q�7@p�u@pQ�@p1'@p�u@p�`@qG�@p�9@p1'@o�;@o�@o
=@n��@nff@m��@m@m@m?}@l�D@lj@lj@lZ@l9X@k��@k�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�?}A�E�A�I�A�I�A�I�A�I�A�G�A�G�A�A�A�5?A�&�A�oA��AָRAփA��AծAՕ�A�p�A�l�A�K�A���Aԥ�A�+AʼjA�r�A���A�1'A�$�A���A��A��HA�E�A���A��DA���A��DA��A���A�1'A�t�A���A�
=A�1'A���A��9A��A�5?A�ȴA�jA��^A�hsA��jA�;dA��-A�"�A���A�{A��DA�?}A��A�-A��\A��!A�ĜA���A�&�A��RA��+A�~�A�VA���A��uA�&�A���A��A� �A��A��A���A� �A�jA�-A�7LA�ĜA�A���A� �A��A�G�A�|�A���A���A�G�A~  A|�`A|1'A{��Az��AzM�Ax�jAw33Avz�At�jAs�Ar�uAq7LApffAo�Ao33Am;dAk��Aj��Ai��Ag�#Ag�Ag%Af�`Af�DAfjAe�FAe�Ad��AdM�Ac�wAcdZAbM�Aal�A_
=A\JAZ�HAY�TAYK�AX^5AW�AU��ASx�AS�AR��AR�DARJAP�DAN�yAM��AL�DAKl�AJ��AI�mAH��AHJAG��AF��AE�hAD�9ADn�AD �AC�PAB�AB��AB�A@��A?��A>�A>(�A<jA:�yA:�A9��A8jA8  A7�A6��A5l�A4�uA3%A2�DA2�A1+A0��A0�A/�-A/A.A�A-hsA,jA*�A)ƨA)�A(M�A'�A'O�A&��A%�hA%;dA$  A#C�A"��A"jA"^5A"E�A"-A"JA!`BA ��A r�A�Ap�A��AQ�A\)Av�A{Ap�A/A�jA�+A5?A��AVAAO�A�`AjAl�A+A��A{AG�A �A&�A�uAA9XA%A
ffA	�A	&�A��AVA��A��A��AA�A\)Ar�A�/A�!A{Ax�A �@�@���@��@�A�@�9X@�  @��m@��F@�@�@�ȴ@���@�(�@��@��@�G�@� �@�!@��@��`@�D@�@�v�@�  @�!@��D@ޟ�@�7L@�l�@��@�@׾w@֏\@�J@���@�G�@�j@�ȴ@��/@�@̛�@�1'@�ƨ@�"�@�hs@Ȭ@�  @���@þw@§�@�7L@�9X@�\)@�~�@��@�|�@�ȴ@�=q@�J@��-@��u@�(�@��P@��@��@�/@�(�@��@�=q@��#@��@�+@�$�@��#@��^@���@��7@��@�x�@�?}@�  @�S�@�+@�ȴ@�ff@�@���@�p�@�V@�Ĝ@���@�(�@�|�@�o@�~�@��#@�O�@��j@��/@��@��D@�Z@��
@��y@��\@��@�hs@�%@���@���@�j@���@���@���@��7@��@��@�+@���@��9@�1'@�C�@��+@��^@���@���@�`B@���@��@�z�@�Q�@��
@�33@��@�n�@�$�@���@���@��^@��-@�p�@��@���@���@�A�@��m@�o@��H@���@��+@�^5@�$�@���@���@�hs@�O�@�%@�z�@�(�@��
@���@�dZ@�;d@��@�@��y@�ȴ@�n�@�-@���@�p�@�V@���@�1'@� �@�1@���@���@��@��+@�^5@�V@��@�X@���@��/@��9@��@�dZ@�t�@�\)@���@�v�@�E�@��@��7@�x�@�hs@��@���@��9@��D@�r�@��/@���@��u@�b@�@l�@+@~�R@}�@~E�@~{@|��@|�j@{�
@z�\@z�@y��@yG�@xĜ@x�u@x �@w|�@v��@v��@vff@v@u�@t9X@sdZ@r��@r��@r��@s@r~�@r�@q�7@p�u@pQ�@p1'@p�u@p�`@qG�@p�9@p1'@o�;@o�@o
=@n��@nff@m��@m@m@m?}@l�D@lj@lj@lZ@l9X@k��@k�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BVBVBVBVBVBVBVBVBVB\B\BbBbB{B�B#�B1'B2-B49B5?B7LB=qB8RBVB�ZB�/B�B��BƨB�qB�XB�FB�-B�B��B��B��B��B�hB�PB�Bx�Br�Bl�BhsBaHB[#BVBQ�BL�BF�BC�B>wB9XB5?B0!B+B$�B�B�B�BDB1BB��B�B�#B�B��B��B��BǮBĜB�wB�LB�9B�B��B��B�%By�BhsB6FB#�B�BPB
��B
�/B
ȴB
�jB
�!B
��B
��B
��B
�7B
~�B
y�B
t�B
n�B
iyB
]/B
S�B
L�B
@�B
:^B
/B
%�B
�B
�B
{B
+B	��B	��B	�B	�ZB	�;B	�5B	�/B	�#B	�B	�B	��B	��B	��B	ȴB	ŢB	�wB	�XB	�B	��B	�{B	�\B	�DB	�%B	�B	x�B	o�B	n�B	l�B	k�B	hsB	`BB	YB	R�B	K�B	E�B	B�B	?}B	:^B	6FB	33B	/B	(�B	$�B	"�B	!�B	�B	�B	�B	{B	VB	
=B	B	B��B�B�B�fB�B�B��B��B��B��BŢBÖB��B�qB�^B�RB�FB�3B�!B�B��B��B��B��B��B��B��B�uB�bB�VB�DB�7B�1B�1B�1B�1B�+B�%B�B�B�B�B�B~�B{�Bx�Bt�Br�Bp�Br�Br�Bq�Bp�Bn�Bl�Bk�BiyBe`BaHB^5B\)B[#BYBXBVBS�BP�BN�BJ�BG�BF�BD�BC�BB�BA�B@�B>wB=qB<jB<jB:^B8RB7LB6FB5?B33B1'B0!B/B.B-B-B-B,B+B(�B'�B&�B&�B%�B%�B$�B#�B"�B!�B!�B �B�B�B�B�B�B�B�B�B �B�B�B �B!�B#�B$�B%�B%�B%�B$�B"�B!�B!�B �B �B#�B)�B,B.B0!B33B49B6FB8RB8RB5?B33B2-B2-B2-B2-B33B5?B7LB8RB9XB:^B:^B<jB@�BB�BE�BH�BL�BM�BM�BN�BN�BN�BN�BN�BP�BQ�BQ�BR�BT�BVBW
BXBYBZB[#B\)B^5B_;B`BB`BB`BBaHBdZBgmBhsBhsBjBn�Bo�Br�Bt�Bw�By�Bz�B|�B�B�B�B�1B�JB�PB�bB��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�3B�9B�LB�^B�dB�wB�wB�}B��BĜBŢBĜBƨBȴB��B��B��B��B��B��B�B�/B�;B�BB�NB�fB�sB�yB�B�B�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	%B	1B	\B	hB	oB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	%�B	&�B	(�B	+B	.B	0!B	1'B	5?B	;dB	<jB	A�B	D�B	E�B	E�B	E�B	F�B	J�B	K�B	O�B	P�B	VB	ZB	\)B	]/B	`BB	bNB	cTB	dZB	ffB	gmB	gmB	iyB	l�B	m�B	m�B	n�B	p�B	q�B	q�B	r�B	t�B	v�B	v�B	w�B	x�B	y�B	z�B	{�B	~�B	� B	~�B	~�B	~�B	�B	�B	�DB	�bB	�bB	�bB	�hB	�uB	�uB	�{B	�{B	�{B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BVBVBVBVBVBVBVBpBVBvB�B�B�BBB$�B1[B2|B4TB5�B8�B@�B@4B�B�'B�B��B��B�#B�;B�B�B�+B��B�mB�|B��B��B�{B�&B��Bz�Bt�BnBjBcB\�BW
BR�BN"BG�BEB?�B:�B6zB1[B,=B%�B �B�BEBB
XB_B��B�IB�B�mB�@B�oB� B�fBŢB�}B�B��B��B�TB��B�1B|�BnB8�B%`B�BHB
��B
��B
�rB
�(B
��B
��B
� B
�/B
��B
�B
z�B
u�B
o�B
kQB
^�B
U2B
N�B
A�B
;�B
0�B
&�B
 �B
�B
�B
�B	��B	�lB	�B	�,B	�pB	ބB	�~B	یB	��B	ּB	�oB	ЗB	̈́B	ɆB	�B	�4B	�PB	�/B	�/B	��B	�bB	��B	��B	�B	z�B	p!B	o B	m)B	lqB	jeB	bB	Z�B	T{B	M6B	F�B	C�B	@�B	;dB	7B	4TB	0�B	)�B	%`B	#nB	"�B	pB	=B	�B	B	�B	^B	�B	B�lB�CB��B�B��B�B��B�{B�B�~B�?B�gBB�BB��B�	B�LB�TB�[B�}B��B�LB��B��B��B�+B��B��B�4B��B�JB��B��B�KB�KB�fB��B�B��B��B��B��B��B�B}<By�ButBs�BqBsMBr�BrGBqvBo�BnBlWBj�Bh
Bb�B^�B\�B\BZQBYBW?BT�BR:BP�BLJBH�BG_BE�BD3BCaBB[BAoB?.B>BB=B=<B;�B9�B7�B7LB6zB5B2�B1�B0oB.}B-CB-]B-]B,�B,WB*eB(�B'�B'�B&�B&�B%zB$�B#�B"hB"NB!HB �B �B BB �B!BB�B �B!HB �B B!�B"4B$&B%�B&�B'8B'mB&�B#�B"4B"NB!bB!�B$�B*�B-�B/ B0�B4B5B6�B9$B9XB6FB3�B2�B2aB2�B2�B3�B5�B7�B9	B9�B;B;B=B@�BC{BF�BIRBMBNBM�BOBN�BOBOBBO�BQhBRBR:BS@BUMBVSBWYBXEBYKBZkB[�B\�B^�B_�B`�B`�B`�BaHBdtBg�Bh�Bh�BkBo Bp!Br�BuBw�By�B{0B}qB��B�gB��B��B��B�"B��B�+B�B�IB�hB�`B��B��B�$B�_B�"B�=B�]B�cB��B�hB��B�fB�xB�B��B��B��B��BĶBżB��B��B�RB�B��B��B�B�B�2B�KB�dB�pB��B�B�B�B�B��B��B�B�B��B��B��B�B��B�$B�<B�cB	GB	B	B	YB	tB	�B	vB	�B	�B	�B	�B	�B	�B	�B	B	�B	�B	�B	B	B	 �B	!�B	# B	#�B	%�B	'B	)*B	+B	./B	0B	1B	5tB	;dB	<�B	A�B	D�B	E�B	E�B	E�B	F�B	J�B	L0B	O�B	Q4B	VmB	ZQB	\CB	]dB	`vB	bhB	cnB	d�B	f�B	g�B	g�B	i�B	l�B	m�B	m�B	n�B	p�B	q�B	q�B	r�B	t�B	wB	v�B	w�B	x�B	y�B	z�B	{�B	.B	�4B	.B	B	.B	�AB	�9B	�xB	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<h�<L��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.06(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910210032172019102100321720191021003217202211182140382022111821403820221118214038201910300108492019103001084920191030010849  JA  ARFMdecpA19c                                                                20191011003704  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191010153708  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191010153710  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191010153711  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191010153712  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191010153712  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191010153712  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20191010153712  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20191010153712  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191010153712  QCF$                G�O�G�O�G�O�            8000JA      jafc1.0                                                                 20191010153713                      G�O�G�O�G�O�                JA  ARUP                                                                        20191010155408                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20191010153223  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20191010153211  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20191020153217  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191020153217  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191029160849  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124038  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                