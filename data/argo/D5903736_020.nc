CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:16Z AOML 3.0 creation; 2016-05-31T19:14:27Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230516  20160531121427  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_020                   2C  D   APEX                            5368                            041511                          846 @�d��p��1   @�d�ax@@3��E����dwKƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dy� D�  D�C3D���D��3D��D�6fD�s3D���D�	�D�33D��3D��3D�fD�C3DچfD�ٚD�3D�<�D�i�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�@�A z�A>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B���B��)B��)B�\B�\B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CX�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cv�Cw�Cy�C{�C}�C�C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��DtUDy{�D��D�@�D���D���D�
�D�4)D�p�D��]D�]D�0�D���D���D�)D�@�Dڄ)D��]D��D�:�D�g]D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�ĜA�A�ĜA�ȴA�ȴA���A���A�ƨA�ƨA�ȴA��HA���A���A�VA���A�ȴAڙ�A�t�A�XA�&�A�JA��A���Aٰ!A�ffA�33A��yA��#A���A�ƨA�x�A� �A�ĜA��A�A��AΧ�A�7LAȩ�AƼjA�1A�ZA���A��yA���A�;dA���A��HA��HA�?}A�7LA���A��A��RA��A���A��`A�n�A�1A�v�A��A�$�A�VA���A��PA���A��DA��FA���A�;dA���A���A�G�A��A��A��A�%A��A�O�A�\)A��HA�(�A��\A�bA�v�A��RA�ƨA�l�A�$�A�A��\A��jA�v�A�ĜA�&�A��^A�hsA���A�7LA�+A��#A�dZA�K�A���A���A��RA��A� �At�A}33Ay�Ax�uAw��AvAtE�Ar��Aq�Ao%Ak��AiG�Ag�Ag
=Ae�Ad=qAb�uA_��A^ �A[l�AX  AT��AS�AR5?AQ�hAP�/AN9XAMAMp�AMO�ALM�AJ�\AIS�AH�\AGC�AFAD�RADAA�TA@�`A@=qA>��A> �A=A;�-A9�#A9dZA8�A6�yA5`BA4�+A3�hA0�A/K�A.��A.$�A-"�A,VA+33A(�HA((�A'��A%l�A#�;A"Q�A!|�A ��A�HA�DAAVAn�A�AJA��Az�A5?A1A�
AdZA��A�wA�DA��AȴA�hA
=A�DA�^A
��A	��A	7LA"�A�-A��A9XA�#AdZA=qA�A ĜA b@�@��/@�9X@�\)@��7@���@�J@���@�C�@�@�Q�@�-@���@�P@�7@�bN@��y@��@�F@��@��@�&�@��@��@޸R@�@܃@ۍP@�?}@�dZ@ղ-@ԋD@��;@Ӿw@�(�@��H@с@�O�@�r�@�1@�+@͙�@̬@�  @ˮ@� �@˾w@�o@���@š�@ģ�@�Q�@Ý�@�@�%@�33@��+@��@�J@�J@��#@���@��P@�"�@��\@�O�@�1@�  @�33@���@���@���@���@��9@��@��j@���@���@�1'@�+@��@��\@��T@�@�&�@���@���@���@�t�@�C�@�"�@�ȴ@�^5@��@��^@�x�@�?}@�/@�%@��u@��@�I�@�\)@�$�@�@��7@��@�Ĝ@�Q�@��
@��;@��
@���@��@��@���@��+@�5?@��@�J@���@���@�`B@��@���@��u@��u@�z�@��m@�|�@��@��R@�~�@��@�&�@��u@���@�@���@�~�@�=q@�-@�-@�-@�$�@�$�@�J@���@���@�@��-@��h@�?}@���@��`@�Ĝ@��9@��u@�z�@�r�@�bN@��@�t�@�dZ@�S�@�C�@�+@�
=@���@��y@��!@�ff@�^5@�M�@�=q@�@��@���@�hs@���@�Ĝ@��D@�Z@� �@�1@��w@�l�@�+@�
=@���@��+@�v�@�-@��T@��#@�@��h@�O�@���@�j@�I�@�(�@�b@��;@��w@�\)@��@���@�ff@�V@�=q@��@��@��T@���@���@�hs@�7L@�V@���@��u@�bN@�(�@���@���@�C�@�@���@�v�@��#@��@�`B@�7L@��@�%@���@���@��@��/@�Ĝ@���@�A�@��@��@���@�+@���@��+@�^5@�5?@��@���@���@�x�@�X@�&�@��9@�r�@�Z@�1@��w@���@�S�@�33@���@���@�M�@�-@��^@�?}@��@���@���@�Z@� �@���@�ff@y�7@q��@f��@\j@R=q@KS�@Bn�@<�@7+@2~�@.v�@+"�@&��@!�#@(�@+@�F@ �@��@	7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A�ĜA�A�ĜA�ȴA�ȴA���A���A�ƨA�ƨA�ȴA��HA���A���A�VA���A�ȴAڙ�A�t�A�XA�&�A�JA��A���Aٰ!A�ffA�33A��yA��#A���A�ƨA�x�A� �A�ĜA��A�A��AΧ�A�7LAȩ�AƼjA�1A�ZA���A��yA���A�;dA���A��HA��HA�?}A�7LA���A��A��RA��A���A��`A�n�A�1A�v�A��A�$�A�VA���A��PA���A��DA��FA���A�;dA���A���A�G�A��A��A��A�%A��A�O�A�\)A��HA�(�A��\A�bA�v�A��RA�ƨA�l�A�$�A�A��\A��jA�v�A�ĜA�&�A��^A�hsA���A�7LA�+A��#A�dZA�K�A���A���A��RA��A� �At�A}33Ay�Ax�uAw��AvAtE�Ar��Aq�Ao%Ak��AiG�Ag�Ag
=Ae�Ad=qAb�uA_��A^ �A[l�AX  AT��AS�AR5?AQ�hAP�/AN9XAMAMp�AMO�ALM�AJ�\AIS�AH�\AGC�AFAD�RADAA�TA@�`A@=qA>��A> �A=A;�-A9�#A9dZA8�A6�yA5`BA4�+A3�hA0�A/K�A.��A.$�A-"�A,VA+33A(�HA((�A'��A%l�A#�;A"Q�A!|�A ��A�HA�DAAVAn�A�AJA��Az�A5?A1A�
AdZA��A�wA�DA��AȴA�hA
=A�DA�^A
��A	��A	7LA"�A�-A��A9XA�#AdZA=qA�A ĜA b@�@��/@�9X@�\)@��7@���@�J@���@�C�@�@�Q�@�-@���@�P@�7@�bN@��y@��@�F@��@��@�&�@��@��@޸R@�@܃@ۍP@�?}@�dZ@ղ-@ԋD@��;@Ӿw@�(�@��H@с@�O�@�r�@�1@�+@͙�@̬@�  @ˮ@� �@˾w@�o@���@š�@ģ�@�Q�@Ý�@�@�%@�33@��+@��@�J@�J@��#@���@��P@�"�@��\@�O�@�1@�  @�33@���@���@���@���@��9@��@��j@���@���@�1'@�+@��@��\@��T@�@�&�@���@���@���@�t�@�C�@�"�@�ȴ@�^5@��@��^@�x�@�?}@�/@�%@��u@��@�I�@�\)@�$�@�@��7@��@�Ĝ@�Q�@��
@��;@��
@���@��@��@���@��+@�5?@��@�J@���@���@�`B@��@���@��u@��u@�z�@��m@�|�@��@��R@�~�@��@�&�@��u@���@�@���@�~�@�=q@�-@�-@�-@�$�@�$�@�J@���@���@�@��-@��h@�?}@���@��`@�Ĝ@��9@��u@�z�@�r�@�bN@��@�t�@�dZ@�S�@�C�@�+@�
=@���@��y@��!@�ff@�^5@�M�@�=q@�@��@���@�hs@���@�Ĝ@��D@�Z@� �@�1@��w@�l�@�+@�
=@���@��+@�v�@�-@��T@��#@�@��h@�O�@���@�j@�I�@�(�@�b@��;@��w@�\)@��@���@�ff@�V@�=q@��@��@��T@���@���@�hs@�7L@�V@���@��u@�bN@�(�@���@���@�C�@�@���@�v�@��#@��@�`B@�7L@��@�%@���@���@��@��/@�Ĝ@���@�A�@��@��@���@�+@���@��+@�^5@�5?@��@���@���@�x�@�X@�&�@��9@�r�@�Z@�1@��w@���@�S�@�33@���@���@�M�@�-@��^@�?}@��@���@���@�Z@� �G�O�@�ff@y�7@q��@f��@\j@R=q@KS�@Bn�@<�@7+@2~�@.v�@+"�@&��@!�#@(�@+@�F@ �@��@	7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�B��B��B��BBJBuB�B#�B33B<jBG�BN�BS�BVBVBVBO�B+B�^Bt�BhsB��BB�B^5BVBM�BF�B@�B>wB8RB33B0!B33B2-B33B7LB:^B;dB>wB=qB=qB>wB?}B@�B?}B>wB9XB1'B'�B �B%BȴB�9B��B��B�%B~�Bs�B`BBYBK�B<jB;dB8RB.B �B�BB��B�B�ZB��BǮB��Bt�BbNB]/BT�B1'B1'B2-B-B%�BPB
�B
�
B
��B
��B
ǮB
��B
�'B
��B
�uB
�B
k�B
ZB
E�B
=qB
5?B
(�B
�B
{B
	7B	��B	�`B	�B	��B	ɺB	��B	�FB	�B	��B	�uB	�B	s�B	gmB	aHB	YB	S�B	K�B	=qB	8RB	6FB	49B	/B	,B	%�B	 �B	�B	VB	1B	B��B	+B	B��B��B��B�B�B�B�yB�fB�BB�)B�B��B��BǮBĜBB�}B�^B�3B�B�B��B��B��B��B��B�uB�PB�7B�+B�B~�B|�Bz�By�By�By�Bx�Bw�Bu�Br�Bp�Bo�Bm�BjBiyBgmBe`BdZBbNB`BB`BB]/B\)B[#BZBYBYBYBZBZB[#BZBYBYBW
BS�BO�BO�BN�BM�BM�BN�BO�BQ�BR�BQ�BQ�BQ�BR�BT�BS�BR�BQ�BR�BVBT�BT�BT�BT�BR�BW
B[#BZB^5Be`BaHB_;BbNBffBgmBgmBhsBhsBiyBo�B}�B~�B}�Bx�Bp�Bv�Bw�By�B|�B|�B}�B�B�B�+B�7B�DB�oB��B��B��B��B��B��B�?B�qB�}B��B��B��B��B�B�/B�HB�ZB�mB�B�B�B��B��B��B	+B		7B	
=B	DB	DB	JB	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	-B	/B	2-B	7LB	:^B	>wB	>wB	>wB	@�B	F�B	H�B	K�B	K�B	M�B	M�B	M�B	N�B	P�B	T�B	ZB	]/B	]/B	]/B	^5B	cTB	gmB	jB	l�B	m�B	m�B	q�B	v�B	z�B	}�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�1B	�7B	�DB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�?B	�RB	�XB	�^B	�jB	�jB	�jB	�wB	��B	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�/B	�;B	�;B	�BB	�BB	�TB	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
PB
VB
VB
VB
\B
\B
bB
uB
oB
�B
!�B
(�B
)�B
1'B
49B
;dB
?}B
E�B
K�B
O�B
S�B
YB
aHB
gmB
k�B
o�B
u�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�*B�B��B��B��BBTB~B�B#�B3<B<tBG�BN�BTBVBVBVBO�B+
B�dBt�BhzB��BB�B^=BVBM�BF�B@�B>B8[B3;B0+B3?B26B3?B7UB:fB;kB>�B=zB={B>�B?�B@�B?�B>~B9_B12B'�B �B.BȽB�@B��B��B�,BBs�B`JBYBK�B<mB;kB8YB.B �B�BB��B�B�aB�BǵB��Bt�BbYB]5BUB12B1.B27B-B%�BXB
�B
�B
��B
��B
ǷB
��B
�2B
��B
��B
�B
k�B
Z+B
E�B
=B
5LB
)B
�B
�B
	FB	��B	�oB	�$B	��B	��B	��B	�WB	�B	��B	��B	�-B	s�B	g�B	aZB	Y-B	TB	K�B	=�B	8hB	6]B	4NB	/3B	,B	%�B	 �B	�B	oB	JB	)B�B	BB	4B�B�B��B��B�B�B�B�B�^B�DB�+B��B��B��BĸB«B��B�{B�MB�8B�B��B��B��B��B��B��B�mB�UB�JB�+BB}
B{ By�By�By�Bx�Bw�Bu�Br�Bp�Bo�Bm�Bj�Bi�Bg�Be�BdxBbmB`bB`cB]NB\JB[CBZ<BY9BY5BY8BZ=BZ:B[BBZ<BY6BY6BW+BTBO�BP BN�BM�BM�BN�BO�BRBSBRBRBRBSBUBTBSBR
BSBV'BUBUBUBUBSBW+B[EBZ=B^RBe~BahB_ZBbmBf�Bg�Bg�Bh�Bh�Bi�Bo�B~BB~Bx�Bp�Bv�Bw�By�B}	B}B~B�&B�6B�HB�UB�aB��B��B��B��B��B��B�B�]B��B��B��B�B�B�B�0B�JB�aB�rB�B��B�B��B��B��B�B	CB		NB	
UB	YB	^B	aB	tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	-$B	/1B	2DB	7bB	:tB	>�B	>�B	>�B	@�B	F�B	H�B	K�B	K�B	M�B	M�B	M�B	N�B	P�B	UB	Z1B	]EB	]DB	]EB	^LB	chB	g�B	j�B	l�B	m�B	m�B	q�B	v�B	z�B	~	B	~B	�B	�%B	�.B	�.B	�.B	�,B	�*B	�2B	�:B	�BB	�GB	�KB	�WB	�kB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�&B	�3B	�8B	�<B	�QB	�eB	�iB	�oB	�{B	�|B	�|B	��B	��B	ĬB	ŶB	ƸB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	� B	� B	�!B	�"B	�/B	�9B	�@B	�LB	�LB	�SB	�UB	�dB	�qB	�pB	�~B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B
 B
B
B
B
!B
(B
.B
.B
-B
-B
2B
>B
AB
AB
	GB
	EB

MB

MB

NB
RB
UB
ZB
XB
_B
dB
fB
fB
jB
kB
pG�O�B
~B
�B
!�B
)B
*	B
16B
4HB
;uB
?�B
E�B
K�B
O�B
TB
Y$B
aSB
g|B
k�B
o�B
u�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214282016053112142820160531121428  AO  ARCAADJP                                                                    20140721230516    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230516  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230516  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121428  IP                  G�O�G�O�G�O�                