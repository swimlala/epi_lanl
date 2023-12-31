CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-02-19T18:23:30Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210219182330  20220204114411  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؆�P��I1   @؆���_�@7w�O�;d�c�$�/1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BXffB_��Bg��Bo��Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy� D�
�D�eD�� D��
D�"=D�V�D���D�ӅD��D�UqD���D���D�qD�c3Dڛ�D��
D��D�UD��D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@��\@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB@�BH�BO�RBX�B_Q�BgQ�BoQ�Bw�RB�RB�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��=C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D.�D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9uD9�D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Dg�Dg��Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�DtۅDy��D��D�b�D���D���D�  D�T{D���D��HD�qD�S4D��gD�ʐD�4D�`�Dڙ�D���D��D�R�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A���A���A�  A�A�  A�  A���A���A���A���A���A��A��#A���AԸRAԣ�Aԟ�AԑhAԁA�`BA� �Aӣ�A��`A�oA� �A�r�A���Aŏ\A���A���A��FA�A�A�1'A���A���A��A���A�l�A��HA�t�A�%A��A�;dA�+A���A��uA��A�VA��hA�K�A�33A��DA�A��A�1A��A���A�5?A��A�=qA��mA��A�+A�A�G�A���A���A�\)A�  A�$�A���A�~�A��A�$�A��!A��mA�t�A���A�-A�p�A�hsA��wA�ZA��PA�5?A��+A�7LA�7LA�%A���A�?}A��-A��A�G�A�bA�M�A�dZA��A���A�5?A���A�bA���A~�yAzv�AyhsAu��Ar��Aq�wAo�Al �AhbNAg�#Afv�Ad^5Ac�A`�\A` �A_��A^��A^M�AZ�!AX��AX5?AWG�AV1'AU`BAS�
AR��AR �AQ��AP��ANbNAK��AJ�AJ1'AI�PAF  ACA@��A?�A?O�A<��A:�9A:JA9��A8��A8��A8$�A6��A5\)A4E�A2ffA1�A1K�A0ffA/�A/oA.�yA.�RA.-A-7LA,ȴA,E�A+�A+`BA)�mA)
=A(Q�A'��A&ffA%�wA%oA$^5A#+A!A!�A!oA bNA��A�wA`BA33A=qAȴA  A^5A5?A=qAoAjA�wA^5A�mA�`Az�A1'A�mA��A�A�`AjAdZA�+AE�AbA�7A
ffA
  A��A&�A��An�AƨAx�A+AI�A�hA��A5?A�AA E�@�|�@���@��P@�J@���@���@�;d@���@�{@�hs@���@�@��@���@�Ĝ@��@���@�Z@��#@�A�@�ff@㝲@�v�@��@�r�@��@�x�@ە�@�$�@�(�@�V@���@��;@�n�@љ�@ϕ�@���@�&�@̣�@�Q�@���@��y@ʧ�@�M�@�  @�S�@�33@�"�@Ƨ�@�X@��`@�A�@�t�@��@��@��
@�@��@�r�@��F@���@�J@��;@�o@���@�ff@���@�Ĝ@��m@��H@���@��@���@��@�|�@���@��@��7@�`B@��D@��F@��@�l�@�;d@�o@��!@��#@��@���@�z�@�A�@���@�@���@�M�@�X@���@�r�@��m@�33@��y@���@�ff@�M�@�M�@�V@�V@�^5@�5?@���@�%@���@�A�@��P@�;d@�+@�
=@���@���@��+@�E�@���@��@�@�v�@�5?@�{@�{@���@�I�@��w@�\)@���@���@��\@�~�@�V@�M�@�V@�E�@�$�@�J@�{@�{@���@��#@���@���@���@�(�@�1@�1'@��m@�l�@�l�@�+@�@�~�@�5?@�5?@�-@�@�p�@�O�@�?}@�Ĝ@�Q�@���@���@���@��F@��@��w@���@��P@�t�@�
=@���@�ff@���@�M�@���@�&�@��@��`@��j@��@�I�@� �@��@��@� �@�(�@��m@��@��@�|�@�|�@�dZ@�C�@�+@�o@��y@��@��R@��\@�ff@�E�@�5?@���@���@�O�@�&�@��`@���@�r�@�1'@��F@�S�@��@�@���@��H@���@���@���@��R@��R@���@�M�@�J@�J@���@��7@��@�p�@�x�@�O�@�V@�V@��@��u@�Z@�1@��m@���@��@�K�@�@���@��\@�-@�@�x�@�?}@�%@��@��`@��`@��u@�9X@��;@��@���@�A�@{��@r��@k4�@cF�@[K�@T2�@M�"@GC�@?�[@:�c@5!�@/��@*��@$�@�,@��@6�@�)@�Z@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A���A���A�  A�A�  A�  A���A���A���A���A���A��A��#A���AԸRAԣ�Aԟ�AԑhAԁA�`BA� �Aӣ�A��`A�oA� �A�r�A���Aŏ\A���A���A��FA�A�A�1'A���A���A��A���A�l�A��HA�t�A�%A��A�;dA�+A���A��uA��A�VA��hA�K�A�33A��DA�A��A�1A��A���A�5?A��A�=qA��mA��A�+A�A�G�A���A���A�\)A�  A�$�A���A�~�A��A�$�A��!A��mA�t�A���A�-A�p�A�hsA��wA�ZA��PA�5?A��+A�7LA�7LA�%A���A�?}A��-A��A�G�A�bA�M�A�dZA��A���A�5?A���A�bA���A~�yAzv�AyhsAu��Ar��Aq�wAo�Al �AhbNAg�#Afv�Ad^5Ac�A`�\A` �A_��A^��A^M�AZ�!AX��AX5?AWG�AV1'AU`BAS�
AR��AR �AQ��AP��ANbNAK��AJ�AJ1'AI�PAF  ACA@��A?�A?O�A<��A:�9A:JA9��A8��A8��A8$�A6��A5\)A4E�A2ffA1�A1K�A0ffA/�A/oA.�yA.�RA.-A-7LA,ȴA,E�A+�A+`BA)�mA)
=A(Q�A'��A&ffA%�wA%oA$^5A#+A!A!�A!oA bNA��A�wA`BA33A=qAȴA  A^5A5?A=qAoAjA�wA^5A�mA�`Az�A1'A�mA��A�A�`AjAdZA�+AE�AbA�7A
ffA
  A��A&�A��An�AƨAx�A+AI�A�hA��A5?A�AA E�@�|�@���@��P@�J@���@���@�;d@���@�{@�hs@���@�@��@���@�Ĝ@��@���@�Z@��#@�A�@�ff@㝲@�v�@��@�r�@��@�x�@ە�@�$�@�(�@�V@���@��;@�n�@љ�@ϕ�@���@�&�@̣�@�Q�@���@��y@ʧ�@�M�@�  @�S�@�33@�"�@Ƨ�@�X@��`@�A�@�t�@��@��@��
@�@��@�r�@��F@���@�J@��;@�o@���@�ff@���@�Ĝ@��m@��H@���@��@���@��@�|�@���@��@��7@�`B@��D@��F@��@�l�@�;d@�o@��!@��#@��@���@�z�@�A�@���@�@���@�M�@�X@���@�r�@��m@�33@��y@���@�ff@�M�@�M�@�V@�V@�^5@�5?@���@�%@���@�A�@��P@�;d@�+@�
=@���@���@��+@�E�@���@��@�@�v�@�5?@�{@�{@���@�I�@��w@�\)@���@���@��\@�~�@�V@�M�@�V@�E�@�$�@�J@�{@�{@���@��#@���@���@���@�(�@�1@�1'@��m@�l�@�l�@�+@�@�~�@�5?@�5?@�-@�@�p�@�O�@�?}@�Ĝ@�Q�@���@���@���@��F@��@��w@���@��P@�t�@�
=@���@�ff@���@�M�@���@�&�@��@��`@��j@��@�I�@� �@��@��@� �@�(�@��m@��@��@�|�@�|�@�dZ@�C�@�+@�o@��y@��@��R@��\@�ff@�E�@�5?@���@���@�O�@�&�@��`@���@�r�@�1'@��F@�S�@��@�@���@��H@���@���@���@��R@��R@���@�M�@�J@�J@���@��7@��@�p�@�x�@�O�@�V@�V@��@��u@�Z@�1@��m@���@��@�K�@�@���@��\@�-@�@�x�@�?}@�%@��@��`@��`@��u@�9X@��;@��@���@�A�@{��@r��@k4�@cF�@[K�@T2�@M�"@GC�@?�[@:�c@5!�@/��@*��@$�@�,@��@6�@�)@�Z@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�qB�qB�qB�qB�qB�qB�qB�qB�wB�}B�}B��B��B��BÖBŢBɺB��B��B��B�B�;B�B��B$�B%�B"�B'�B9XBP�B\)Bt�Bq�Bs�Bz�By�B� B�B�=B�hB�oB�oB�uB��B��B��B��B��B�uB�oB�bB�\B�7B�+B�B�B�B{�Bw�Bs�Bs�Bp�Bo�Bl�Bk�Bm�BaHBT�B@�B2-B.B!�BJBB��B�B�NB�B��B��B�RB�B��B�\B|�Bl�BXB=qB%�BuBB
�B
��B
ƨB
�RB
��B
��B
�oB
�B
|�B
w�B
q�B
gmB
^5B
I�B
#�B	��B	�sB	ŢB	��B	��B	�1B	m�B	Q�B	J�B	F�B	>wB	K�B	M�B	T�B	ZB	\)B	\)B	`BB	R�B	R�B	O�B	J�B	G�B	C�B	G�B	K�B	J�B	G�B	?}B	,B	)�B	(�B	&�B	hB�B�B��B��BB�-B�B�B�B��B�B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�bB�\B�\B�VB�JB�DB�7B�1B�B~�Bx�Br�BjBe`BaHB_;B\)BXBR�BP�BO�BN�BN�BN�BM�BK�BJ�BH�BE�BD�BD�BF�BF�BF�BG�BC�BB�BC�BB�BA�BA�BA�B@�B@�B@�B?}B?}B?}B>wB?}B<jB<jB<jB;dB:^B9XB9XB9XB9XB9XB9XB9XB9XB9XB7LB7LB6FB5?B7LB<jB<jB=qB<jB=qB<jB?}BA�BA�BA�B@�BG�BH�BI�BN�BO�BP�BQ�BQ�BQ�BT�BS�BS�B[#B[#B\)B\)B]/B^5B`BBaHBbNBcTBcTBffBhsBhsBhsBjBiyBl�Bq�Bq�Bq�Bq�Bs�Bt�Bu�Bx�B{�B|�B|�B� B�B�B�%B�+B�+B�=B�PB�PB�PB�VB�\B�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�?B�FB�RB�dB�wBBÖBÖBBBÖBƨBɺBɺBɺBɺB��B��B��B��B��B��B�B�TB�`B�`B�`B�fB�TB�fB�B�B��B��B��B��B��B��B	B	%B	%B	+B	DB	VB	\B	uB	�B	�B	$�B	+B	,B	-B	.B	1'B	33B	=qB	C�B	C�B	C�B	D�B	J�B	K�B	L�B	S�B	W
B	[#B	^5B	`BB	aHB	cTB	ffB	hsB	hsB	iyB	m�B	p�B	r�B	s�B	v�B	y�B	}�B	�B	�%B	�%B	�1B	�7B	�DB	�JB	�JB	�PB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�FB	�RB	�XB	�XB	�^B	�jB	�qB	�wB	��B	B	ÖB	ŢB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�)B	�/B	�5B	�5B	�5B	�;B	�NB	�TB	�ZB	�`B	�`B	�B	�JB
	lB
�B
B
"NB
+kB
1�B
:*B
AB
D�B
J#B
P}B
VB
Z�B
a|B
e`B
jKB
m]B
q[B
v111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�nB�nB�nB�nB�nB�nB�nB�nB�tB�zB�zB��B��B��B��B��B��B��B��B��B�B�6B�B��B�B�B�B�B0OBG�BSBk�Bh�Bj�Bq�Bp�Bv�B{B�1B�\B�cB�cB�iB�{B�uB�{B�{B�|B�jB�dB�WB�RB�-B~"B{ByBw�Br�Bn�Bj�Bj�Bg�Bf�Bc�Bb~Bd�BXBBK�B7�B)+B%B�BKB�B��B�B�RB�B��B��B�ZB�#B��B�gBs�Bc�BOB4�B�B
�B
�B
�B
�B
��B
�nB
��B
��B
��B
z2B
tB
n�B
h�B
^�B
UXB
@�B
�B	��B	ߟB	��B	�B	��B	eB	d�B	I$B	A�B	=�B	5�B	C B	EB	L7B	QUB	SaB	SaB	WzB	J,B	J,B	GB	A�B	>�B	:�B	>�B	CB	A�B	>�B	6�B	#FB	!:B	 4B	'B	�B��B�[B�1B�B��B�uB�VB�PB�JB�DB�JB�B�B��B��B��B��B��B�B�B�QB�dB�]B�RB�]B�LB�@B�3B�'B�B��B�B�B��B��B��B��B��B��B��B��B��B��B��BB|mBvHBp$Bj Ba�B\�BX�BV�BS{BObBJEBH8BG2BF,BF,BF,BE&BCBBB@B<�B;�B;�B=�B=�B=�B?B:�B9�B:�B9�B8�B8�B8�B7�B7�B7�B6�B6�B6�B5�B6�B3�B3�B3�B2�B1�B0�B0�B0�B0�B0�B0�B0�B0�B0�B.�B.�B-�B,�B.�B3�B3�B4�B3�B4�B3�B6�B8�B8�B8�B7�B?B@BABF1BG7BH=BIDBIDBIDBLVBKPBKPBR{BR{BS�BS�BT�BU�BW�BX�BY�BZ�BZ�B]�B_�B_�B_�Ba�B`�Bc�BiBiBiBiBkBlBmBp-Bs>BtEBtEBwWBx]B{pB}|B~�B~�B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�'B�>B�EB�KB�]B�pB�vB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�8B�KB�KB�QB�pBڦBܲBܲBܲBݸBڦBݸB��B��B�B�,B�8B�?B�JB�JB�\B�uB�uB�{B	�B	�B	�B	
�B	�B	B	+B	"PB	#VB	$\B	%bB	(tB	*�B	4�B	:�B	:�B	:�B	;�B	BB	CB	DB	KCB	NUB	RnB	U�B	W�B	X�B	Z�B	]�B	_�B	_�B	`�B	d�B	g�B	i�B	j�B	nB	q$B	u=B	zZB	}mB	}mB	yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�)B	�/B	�6B	�NB	�TB	�TB	�ZB	�aB	�gB	�mB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�0B	�6B	�<B	�ZB	�`B	�fB	�lB	�rB	�lB	�rB	�xB	�xB	�xB	�~B	ّB	ڗB	۝B	ܣB	ܣB	�JB	�B
 �B
B
AB
�B
"�B
)B
1iB
8EB
;�B
AbB
G�B
M]B
Q�B
X�B
\�B
a�B
d�B
h�B
mO111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144122022020411441220220204114412  AO  ARCAADJP                                                                    20210219182330    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210219182330  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210219182330  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114412  IP                  G�O�G�O�G�O�                