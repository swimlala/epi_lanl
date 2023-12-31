CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-05-17T15:39:09Z creation;2020-05-17T15:39:13Z conversion to V3.1;2022-11-21T05:27:05Z update;     
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
resolution        =���   axis      Z          :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   B   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       D    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       N<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   VP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       XX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       bt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   j�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       l�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   t�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       v�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       ~�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20200517153909  20221123114512  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_210                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��A�1   @����@;'-�d��t�j1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�C3D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DT�DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du�Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�@�D�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A�?}A��A��uA��A��A�5?A��A���A��A�7LA��A�
=A��/A�ĜA���A�r�A�C�A���A���A��!A�  A��yA�;dA��mA���A��A��A���A�
=A��A��yA�9XA��uA�ĜA��A���A���A�"�A��PA�$�A���A��A��A�bA�O�A��\A�I�A�O�A�  A���A�ZA�/A��HA�l�A�JA�ȴA�A�A�n�A�+A�v�A�  A�5?A�t�A�7LA�jA���A���A�oA�C�A�A��A�S�A�bA�O�A�{A�(�A��A�t�A�XA�jA�ZA�A�jA���A���A��A�/A���A���A�VA���A��A+A~VA~�A|�Az��AzbAyp�Ax~�Av�AtȴAq�
Ap�ApffAn��Am\)Ak�-Ai��Ai��Ahv�Af�DAd1'Ab�yAa�mA`�A^��A^M�A]�TA]|�A\VAZĜAY��AYXAY
=AXE�AV�/AV5?AUXATȴAT��AT^5AS�7AR�RAQ�AQXAP�uAOdZAM�-AL�yALbAK�PAK\)AJ�HAIt�AH��AH$�AG�AF��AE�AD�HADZAC�^AB�`AB�\AA�#A@��A@9XA?t�A=�TA<�A;x�A:�/A: �A9�A9G�A9+A8A�A7dZA6r�A5�FA5p�A4��A3��A2ZA1��A1G�A0��A0z�A0A/x�A.$�A-K�A+�A*�A)C�A(��A( �A'�-A&ffA$�`A#�;A"�A"v�A"JA!`BA ��AA&�A�
A�/A��A��A�jA�uAffAbA��AA+A�AjA�FA�A��A^5AS�A  AVA�jA�RA�A�A��A��A��A�DAffAI�A�hA~�A��A��A�^AG�Ar�AA�-AG�AĜAbAl�A"�AVAE�A�mA�AoA �R@�"�@�X@���@��y@�r�@���@���@�t�@�h@���@��@�\@�v�@�E�@���@�7L@㕁@���@�$�@��@�^5@���@�hs@���@և+@�Q�@��m@ӝ�@Ӆ@�+@Ѻ^@�33@ͩ�@��;@�E�@ɩ�@�/@ț�@Ǯ@ư!@Ų-@ċD@��@+@�@�7L@���@�b@���@�
=@���@���@�^5@���@��@��w@�V@�x�@�/@���@�9X@��@�$�@�?}@�bN@��F@�C�@��@���@��@�r�@��
@�\)@��@�?}@��@�Ĝ@�r�@�Z@�9X@��m@��w@�
=@��!@�ff@��^@�V@��9@��@�r�@�I�@�t�@���@�$�@�?}@�Ĝ@��@�bN@���@�+@�E�@���@�?}@�A�@�|�@��@���@�-@��T@�x�@��@���@��j@�I�@��@���@��@���@�?}@���@�  @��P@��P@�S�@���@��+@�5?@��T@���@��@�z�@��w@��y@�=q@�@���@��7@�p�@�?}@�%@�r�@��@���@�t�@�+@���@�{@�p�@���@�z�@�1@���@�K�@�
=@��@���@���@��^@���@��7@�p�@�X@�7L@��@��@��@��`@���@�9X@�  @��;@��w@���@��@�|�@�;d@�
=@��y@�=q@�x�@���@�r�@�Q�@�(�@��
@�dZ@��@��!@��\@�~�@�V@��#@��h@�x�@���@��D@�bN@�1'@�;@�@��@|�@K�@�@~ȴ@~��@}�h@|I�@{t�@{o@z�H@z^5@z=q@y�@yhs@y�@x��@x�`@x��@xĜ@x�9@xb@w\)@v��@v��@vff@v5?@v{@v@u@u�h@u?}@tz�@t(�@t1@s�
@st�@s"�@r�H@rn�@q��@q��@q�7@qX@p�9@p�@pQ�@o�@n��@n��@m�T@l9X@k�m@kƨ@k33@j�!@j�@i7L@h�9@hb@g��@g
=@f�y@f�@f��@fv�@fff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A�?}A��A��uA��A��A�5?A��A���A��A�7LA��A�
=A��/A�ĜA���A�r�A�C�A���A���A��!A�  A��yA�;dA��mA���A��A��A���A�
=A��A��yA�9XA��uA�ĜA��A���A���A�"�A��PA�$�A���A��A��A�bA�O�A��\A�I�A�O�A�  A���A�ZA�/A��HA�l�A�JA�ȴA�A�A�n�A�+A�v�A�  A�5?A�t�A�7LA�jA���A���A�oA�C�A�A��A�S�A�bA�O�A�{A�(�A��A�t�A�XA�jA�ZA�A�jA���A���A��A�/A���A���A�VA���A��A+A~VA~�A|�Az��AzbAyp�Ax~�Av�AtȴAq�
Ap�ApffAn��Am\)Ak�-Ai��Ai��Ahv�Af�DAd1'Ab�yAa�mA`�A^��A^M�A]�TA]|�A\VAZĜAY��AYXAY
=AXE�AV�/AV5?AUXATȴAT��AT^5AS�7AR�RAQ�AQXAP�uAOdZAM�-AL�yALbAK�PAK\)AJ�HAIt�AH��AH$�AG�AF��AE�AD�HADZAC�^AB�`AB�\AA�#A@��A@9XA?t�A=�TA<�A;x�A:�/A: �A9�A9G�A9+A8A�A7dZA6r�A5�FA5p�A4��A3��A2ZA1��A1G�A0��A0z�A0A/x�A.$�A-K�A+�A*�A)C�A(��A( �A'�-A&ffA$�`A#�;A"�A"v�A"JA!`BA ��AA&�A�
A�/A��A��A�jA�uAffAbA��AA+A�AjA�FA�A��A^5AS�A  AVA�jA�RA�A�A��A��A��A�DAffAI�A�hA~�A��A��A�^AG�Ar�AA�-AG�AĜAbAl�A"�AVAE�A�mA�AoA �R@�"�@�X@���@��y@�r�@���@���@�t�@�h@���@��@�\@�v�@�E�@���@�7L@㕁@���@�$�@��@�^5@���@�hs@���@և+@�Q�@��m@ӝ�@Ӆ@�+@Ѻ^@�33@ͩ�@��;@�E�@ɩ�@�/@ț�@Ǯ@ư!@Ų-@ċD@��@+@�@�7L@���@�b@���@�
=@���@���@�^5@���@��@��w@�V@�x�@�/@���@�9X@��@�$�@�?}@�bN@��F@�C�@��@���@��@�r�@��
@�\)@��@�?}@��@�Ĝ@�r�@�Z@�9X@��m@��w@�
=@��!@�ff@��^@�V@��9@��@�r�@�I�@�t�@���@�$�@�?}@�Ĝ@��@�bN@���@�+@�E�@���@�?}@�A�@�|�@��@���@�-@��T@�x�@��@���@��j@�I�@��@���@��@���@�?}@���@�  @��P@��P@�S�@���@��+@�5?@��T@���@��@�z�@��w@��y@�=q@�@���@��7@�p�@�?}@�%@�r�@��@���@�t�@�+@���@�{@�p�@���@�z�@�1@���@�K�@�
=@��@���@���@��^@���@��7@�p�@�X@�7L@��@��@��@��`@���@�9X@�  @��;@��w@���@��@�|�@�;d@�
=@��y@�=q@�x�@���@�r�@�Q�@�(�@��
@�dZ@��@��!@��\@�~�@�V@��#@��h@�x�@���@��D@�bN@�1'@�;@�@��@|�@K�@�@~ȴ@~��@}�h@|I�@{t�@{o@z�H@z^5@z=q@y�@yhs@y�@x��@x�`@x��@xĜ@x�9@xb@w\)@v��@v��@vff@v5?@v{@v@u@u�h@u?}@tz�@t(�@t1@s�
@st�@s"�@r�H@rn�@q��@q��@q�7@qX@p�9@p�@pQ�@o�@n��@n��@m�T@l9X@k�m@kƨ@k33@j�!@j�@i7L@h�9@hb@g��@g
=@f�y@f�@f��@fv�@fff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B8RB6FB6FB8RB6FB49B49B49B2-B/B+B+B-B.B.B.B.B33BA�BYBjBy�B�B�%B�B�VB��B��B��B��B�!B�'B�!B�B��B��B��B��B��B�uB�bB�\B�PB�JB�1B�B}�Bp�B`BB]/BYBXBVBQ�BM�BI�BE�B<jB+BoBB��B��B�;B��B�LB�!B�B��B�B}�Bx�BgmBaHBI�B8RB)�BuBB
�B
�`B
�
B
��B
ɺB
ÖB
��B
�}B
�XB
�?B
�'B
��B
��B
�oB
�1B
�B
~�B
v�B
gmB
cTB
_;B
W
B
K�B
>wB
,B
$�B
 �B
�B
PB
B	��B	�B	�B	�BB	��B	��B	ĜB	�dB	�9B	�!B	�B	��B	��B	��B	��B	�uB	�hB	�PB	�%B	�B	}�B	z�B	y�B	w�B	s�B	n�B	jB	e`B	`BB	XB	O�B	L�B	H�B	D�B	B�B	>wB	7LB	2-B	/B	-B	(�B	$�B	 �B	�B	�B	�B	�B	oB	JB		7B	B��B��B�B�B�B�B�sB�sB�`B�BB�;B�5B�/B�#B�B��B��B��B��B��B��B��BĜB�}B�RB�-B�B�B��B��B��B��B��B��B��B�uB�bB�PB�DB�+B�B�B}�B{�Bz�By�Bx�Bw�Bu�Br�Bm�BiyBgmBe`BcTBbNB`BB]/B[#BYBYBYBYBYBYBXBXBXBW
BVBS�BQ�BM�BI�BG�BF�BE�BC�BC�BB�BA�B?}B?}B>wB=qB<jB<jB;dB:^B8RB7LB6FB5?B33B2-B0!B/B-B+B(�B+B)�B)�B(�B(�B'�B(�B'�B'�B%�B$�B%�B$�B"�B"�B"�B#�B#�B"�B"�B"�B#�B$�B%�B%�B%�B$�B%�B'�B+B.B/B0!B1'B1'B2-B33B49B49B5?B5?B6FB6FB6FB7LB8RB:^B<jB<jB<jB=qB?}B@�BB�BD�BF�BF�BG�BG�BH�BL�BM�BN�BQ�BT�BT�BVBW
BXBXBYBYBZB[#B\)B]/B_;B`BB`BB`BB`BBbNBdZBffBiyBjBk�Bk�Bn�Bo�Bp�Br�Bs�Bu�Bv�By�Bz�B}�B~�B�B�B�B�%B�1B�=B�VB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B�B�B�!B�?B�RB�^B�jB�jB�qB�}B��BĜBȴB��B��B��B��B�B�)B�;B�HB�TB�`B�mB�yB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	%B	%B	+B		7B	DB	JB	bB	{B	�B	�B	�B	�B	 �B	#�B	%�B	'�B	(�B	)�B	+B	0!B	33B	49B	7LB	9XB	;dB	<jB	>wB	?}B	@�B	@�B	A�B	C�B	E�B	F�B	I�B	M�B	Q�B	S�B	T�B	ZB	\)B	`BB	bNB	e`B	e`B	ffB	ffB	ffB	ffB	k�B	o�B	p�B	p�B	q�B	r�B	r�B	r�B	s�B	u�B	w�B	z�B	{�B	|�B	}�B	� B	�B	�B	�B	�%B	�+B	�1B	�7B	�JB	�PB	�VB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B9rB9�B:DB9rB7fB4�B4�B5B2�B/�B+6B+QB-wB.cB.}B.�B/ B5BDB[�Bm�B|B��B�_B��B��B�dB�eB��B��B��B��B��B�B��B��B��B��B��B�FB��B��B�B��B�=B��B��BtBaHB^BY�BX�BV�BR�BN�BJ�BG+B>wB-�BFB�BoB�rB�B�NB��B��B�}B��B��B}B{Bi*Be,BL�B;B-wB9B�B
��B
�B
�_B
��B
ʦB
�3B
�'B
��B
�*B
�+B
�B
�KB
��B
�aB
�7B
��B
��B
y	B
hXB
dZB
`�B
YeB
N�B
AUB
-wB
%�B
"�B
yB
vB
�B	��B	�tB	�B	��B	ԕB	�JB	ƨB	��B	�%B	��B	��B	��B	��B	��B	�?B	�,B	��B	��B	�B	�B	~�B	{JB	zxB	x�B	t�B	o�B	kkB	f�B	a�B	Y�B	Q B	M�B	IRB	E9B	C{B	@B	8lB	2�B	0B	.B	*eB	&B	!�B	�B	�B	KB	�B	�B	B	
�B	B�VB�lB��B�}B�WB�B��B�B�B�|B�'B�B�jBܒBٚB��BуB�}BϫB��B��B�~B�%B�;B�B�B��B��B��B��B��B�B��B�EB�9B�{B��B��B�~B��B��B�uB~�B|PB{0BzDByrBx�BwLBu%Bo�Bj�BhsBf2Bc�Bc:Ba�B^�B\CBY�BY1BY1BYKBY1BYKBX_BXEBXyBW�BW?BU�BS�BQNBJ�BH�BG�BF?BDMBDMBCaBB�B@OB@B>�B>�B=B=B<jB;JB9�B8�B7B6�B4�B3�B1�B0oB.�B-�B,"B+�B*0B*KB)_B)�B)B)�B(�B(�B(>B'RB&�B%�B$�B$&B#:B$&B$&B#nB$B$�B%B&B'B&�B&�B%zB&�B(�B+�B/ B0B0�B1�B1�B2�B3�B4�B4�B5tB5�B6�B7B7B8B9>B:�B<�B<�B=<B>BB@4BAUBC-BEBG+BGBHBHfBI�BMPBN�BO�BR�BU2BUMBVmBWYBXEBX_BYeBY�BZ�B[�B\�B]�B_�B`�B`vB`�B`�Bb�Bd�BgBi�Bj�Bk�Bl=Bo5BpUBq'Bs3Bt�Bv`BwLBz*B{JB~BB}B��B�MB�SB��B��B�B��B��B��B��B�9B��B��B�B�BB�B� B�FB�LB�mB��B��B��B��B��B��B��B��B��B��B� B�B�B��B�(B�}B�uBخBܒBߤB��B�B��B�B��B��B�B��B��B��B��B�B��B��B�B�B�HB	 iB	[B	GB	GB	3B	9B	YB	?B	_B		�B	�B	�B	B	�B	�B	�B	�B	B	!-B	$&B	&LB	($B	)*B	*0B	+kB	0UB	3hB	4�B	7�B	9�B	;�B	<�B	>�B	?�B	@�B	@�B	A�B	C�B	E�B	GB	J=B	N"B	R B	T,B	U2B	ZQB	\xB	`vB	bhB	ezB	ezB	f�B	f�B	f�B	f�B	k�B	o�B	p�B	p�B	q�B	r�B	r�B	r�B	s�B	vB	xB	z�B	|B	}"B	~(B	�B	�;B	�[B	�gB	�?B	�EB	�fB	�lB	�dB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�,B	�2B	�>B	�
B	�
B	�B	�0B	�B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202005280034512020052800345120200528003451202211182143072022111821430720221118214307202005290020002020052900200020200529002000  JA  ARFMdecpA19c                                                                20200518003811  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200517153909  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200517153911  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200517153912  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200517153912  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200517153912  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200517153912  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200517153912  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200517153912  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200517153912  QCF$                G�O�G�O�G�O�            8000JA      jafc1.0                                                                 20200517153913                      G�O�G�O�G�O�                JA  ARUP                                                                        20200517155419                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200517153523  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200517153509  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20200527153451  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200527153451  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200528152000  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124307  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114512                      G�O�G�O�G�O�                