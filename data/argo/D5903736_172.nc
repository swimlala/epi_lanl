CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:55Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181121041155  20190604094025  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @���s�8�1   @����2V@46ȴ9X�d�l�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�=D�RD�@�D�mqD��\D��D�FfD��\D��D��D�E�D�q�D��D�D�R�D�h D��HD�
D�NfD�g
D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @5�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C �C!�zC#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D��D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DD�DD��DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Dy��D�D�>gD�k4D��D��D�D)D��D���D��D�C�D�o�D���D��D�P�D�e�D��D��D�L)D�d�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A͏\A�Q�A�bA�
=A�1A���A��A��`A��;A��A���A���A���A���A���A���A���A���A���A���A�ƨA�A̼jA̺^A̴9A̲-A̰!A̬A̴9A�ƨA�ĜA�$�A�ȴA˟�A�\)A�-A���A��/A�ĜAʣ�Aʉ7A�n�A�O�A�?}A�33A�&�A��A�1A��A���AɼjAɬAɝ�AɑhA�p�A�XA�Q�A�O�A�C�A�?}A�A�A�33A�1'A���A�XAǕ�A��A��TAƗ�A�S�A�bAť�A�1A�ĜA�M�Aú^A�O�A��yA¶FA���A���A���A�hsA�^5A��A��A�XA�"�A�1A��/A�?}A��A��hA��A��PA�ĜA��yA��wA���A�bA��FA���A�r�A�?}A���A���A��A��A�&�A�XA���A�-A���A�O�A�JA�l�A�XA�+A��yA���A���A��uA�A�dZA�t�A���A���A��hA���A�|�A���A�ĜA�
=A�A~ �A}p�A}%A|��A|ĜA|��A|n�Az~�Ax �Av$�At�At5?As��AsXAo\)Am\)Aj��Ah-Adv�Ac7LAb1'A_�A[O�AX-AU��AR�DAQ?}AO��AM��AM�AK��AJM�AGdZAE;dAAG�A?�FA?;dA>�A;33A9p�A8�A6{A4��A4bNA2��A0��A/`BA. �A-ƨA-��A,�`A+�
A+�A*�uA)�A'XA$��A#��A#��A#�A"�yA!�A!&�A\)A�HA�RAv�A1'A�AffAE�A�
AAl�A��A�A��A�An�A��A�A"�A��A7LA�/A�
A��A��A�A;dA
�9A
A	��A�!A{A�A�/A��Ap�A�yA�A�A��A�/A ��@�S�@���@�@��@�~�@�I�@�t�@���@�^5@�@��@�bN@��;@���@�P@���@��^@�&�@���@�C�@��@��T@�7L@���@�Ĝ@�D@�r�@��;@�S�@�ff@��`@�C�@���@�~�@��@��`@��@���@�  @�C�@�@�ȴ@֗�@�E�@�@ղ-@�p�@�?}@��@�A�@���@�`B@��/@�9X@ύP@�@�&�@�ff@ǍP@��@�ȴ@��@ě�@�@��@�Z@�dZ@�~�@��#@��#@��-@�O�@�?}@�7L@�/@�bN@�ƨ@�t�@��@�|�@�+@�33@���@�ȴ@���@�n�@���@��@�O�@�&�@��@��@���@���@�33@�M�@��T@�z�@��@��+@�@�X@� �@���@�\)@��H@���@���@��@�x�@��@��/@�A�@�l�@�$�@���@�G�@��@�bN@�1@���@�
=@�ff@���@��7@�/@��@���@�z�@�Q�@�(�@���@��P@�ȴ@�J@��T@�@�O�@��@�%@���@��j@���@��9@�Ĝ@���@��/@��@�G�@�/@�V@���@���@��`@��`@�bN@�I�@�(�@�1@�  @��;@���@��@��w@�ƨ@��@���@���@���@��P@�dZ@�
=@��\@���@��@�b@��@��F@��@��@�1@��m@���@��@�l�@�@��R@�5?@�J@���@���@��@��T@���@��-@���@�`B@�O�@�/@�V@��@���@��@�r�@�Z@�1'@� �@��@�1@���@�n�@��\@�n�@�@��@��D@�b@��w@�@���@�~�@�M�@�E�@�v�@���@��y@��@��@���@��@��+@���@�G�@�/@��j@��j@�Ĝ@��u@�I�@���@�t�@�l�@���@��!@�~�@�V@�@�`B@��/@��9@�A�@���@�0�@���@t��@lz�@d(�@]��@VE�@N��@H[�@@<�@7�@02�@,`�@%}�@!�.@�$@>B@�@?�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A͏\A�Q�A�bA�
=A�1A���A��A��`A��;A��A���A���A���A���A���A���A���A���A���A���A�ƨA�A̼jA̺^A̴9A̲-A̰!A̬A̴9A�ƨA�ĜA�$�A�ȴA˟�A�\)A�-A���A��/A�ĜAʣ�Aʉ7A�n�A�O�A�?}A�33A�&�A��A�1A��A���AɼjAɬAɝ�AɑhA�p�A�XA�Q�A�O�A�C�A�?}A�A�A�33A�1'A���A�XAǕ�A��A��TAƗ�A�S�A�bAť�A�1A�ĜA�M�Aú^A�O�A��yA¶FA���A���A���A�hsA�^5A��A��A�XA�"�A�1A��/A�?}A��A��hA��A��PA�ĜA��yA��wA���A�bA��FA���A�r�A�?}A���A���A��A��A�&�A�XA���A�-A���A�O�A�JA�l�A�XA�+A��yA���A���A��uA�A�dZA�t�A���A���A��hA���A�|�A���A�ĜA�
=A�A~ �A}p�A}%A|��A|ĜA|��A|n�Az~�Ax �Av$�At�At5?As��AsXAo\)Am\)Aj��Ah-Adv�Ac7LAb1'A_�A[O�AX-AU��AR�DAQ?}AO��AM��AM�AK��AJM�AGdZAE;dAAG�A?�FA?;dA>�A;33A9p�A8�A6{A4��A4bNA2��A0��A/`BA. �A-ƨA-��A,�`A+�
A+�A*�uA)�A'XA$��A#��A#��A#�A"�yA!�A!&�A\)A�HA�RAv�A1'A�AffAE�A�
AAl�A��A�A��A�An�A��A�A"�A��A7LA�/A�
A��A��A�A;dA
�9A
A	��A�!A{A�A�/A��Ap�A�yA�A�A��A�/A ��@�S�@���@�@��@�~�@�I�@�t�@���@�^5@�@��@�bN@��;@���@�P@���@��^@�&�@���@�C�@��@��T@�7L@���@�Ĝ@�D@�r�@��;@�S�@�ff@��`@�C�@���@�~�@��@��`@��@���@�  @�C�@�@�ȴ@֗�@�E�@�@ղ-@�p�@�?}@��@�A�@���@�`B@��/@�9X@ύP@�@�&�@�ff@ǍP@��@�ȴ@��@ě�@�@��@�Z@�dZ@�~�@��#@��#@��-@�O�@�?}@�7L@�/@�bN@�ƨ@�t�@��@�|�@�+@�33@���@�ȴ@���@�n�@���@��@�O�@�&�@��@��@���@���@�33@�M�@��T@�z�@��@��+@�@�X@� �@���@�\)@��H@���@���@��@�x�@��@��/@�A�@�l�@�$�@���@�G�@��@�bN@�1@���@�
=@�ff@���@��7@�/@��@���@�z�@�Q�@�(�@���@��P@�ȴ@�J@��T@�@�O�@��@�%@���@��j@���@��9@�Ĝ@���@��/@��@�G�@�/@�V@���@���@��`@��`@�bN@�I�@�(�@�1@�  @��;@���@��@��w@�ƨ@��@���@���@���@��P@�dZ@�
=@��\@���@��@�b@��@��F@��@��@�1@��m@���@��@�l�@�@��R@�5?@�J@���@���@��@��T@���@��-@���@�`B@�O�@�/@�V@��@���@��@�r�@�Z@�1'@� �@��@�1@���@�n�@��\@�n�@�@��@��D@�b@��w@�@���@�~�@�M�@�E�@�v�@���@��y@��@��@���@��@��+@���@�G�@�/@��j@��j@�Ĝ@��u@�I�@���@�t�@�l�@���@��!@�~�@�V@�@�`B@��/@��9G�O�@���@�0�@���@t��@lz�@d(�@]��@VE�@N��@H[�@@<�@7�@02�@,`�@%}�@!�.@�$@>B@�@?�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBs�Bs�Bs�Bs�Bs�Br�Bq�Bq�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bo�Bp�Bp�Bp�Bp�Bq�Bu�B�B�oB��B�LB�!B�FBÖB��B�BB�mB�B�B��BB+BJB\BhBuB�B�B�B�B �B!�B"�B#�B&�B'�B'�B(�B)�B(�B)�B+B-B1'B5?B@�BL�BR�BYB]/B_;Be`Bm�Bn�Bp�Br�Bs�Bt�Bs�Bw�B}�B�uB�DB�JB��B��B�-B�XB�RB�?B�9B�LB�'B��B��Bu�B[#BXBQ�B;dB�BhB��B�HB�B�HB�fB�fB�NB�B��BɺB��B�-B��B��B�7Br�BVBD�B9XB)�B �B�BDBB
��B
��B
�fB
�B
�jB
�B
��B
��B
�=B
�1B
�+B
�%B
�B
�B
� B
r�B
_;B
M�B
A�B
<jB
9XB
2-B
�B
B	�B	�NB	��B	B	�XB	��B	�VB	z�B	l�B	^5B	VB	M�B	E�B	@�B	9XB	2-B	#�B	�B	B��B��B�B�B�fB�NB�;B�)B�B�
B��B��B��B��B��B��B��B��B��BȴBƨBȴB��B��B��B��B��BɺBɺBȴBǮBƨBÖB��B�wB��B�}B�}B�wB�qB�jB�dB�^B�XB�LB�?B�-B�B�B�B�B�B�B�B�B�B�B�B�?B�XB�^B�jB�jB�jBÖB��B�
B��BŢB�?B�B��B��B�oB�=B�B�B�B�B�%B�%B�+B�+B�+B�+B�+B�+B�bB�bB�bB�\B�\B�\B�bB�bB�bB�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�LB�LB�^B�wB��B��B��B�
B�#B�;B�NB�ZB�mB�sB�B�B��B��B��B	B	+B		7B	bB	uB	{B	{B	�B	�B	�B	�B	 �B	$�B	$�B	 �B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	%�B	)�B	.B	1'B	33B	6FB	8RB	;dB	;dB	<jB	>wB	?}B	@�B	A�B	E�B	H�B	J�B	K�B	L�B	M�B	N�B	N�B	N�B	O�B	O�B	N�B	R�B	W
B	XB	YB	\)B	`BB	dZB	jB	m�B	p�B	s�B	t�B	w�B	y�B	� B	�7B	�JB	�bB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�LB	�dB	�wB	��B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	�
B	�B	�#B	�5B	�;B	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�fB	�B	�B	�B	�B	�B	�B	�B	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
	7B
 �B
HB
B
"�B
/5B
72B
=�B
B�B
IRB
N�B
T�B
[�B
a�B
e�B
lB
m�B
rGB
u�B
x�B
|�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 Bm�Bm�Bm�Bm�Bm�Bl�Bk�Bk�Bk�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bi�Bj�Bj�Bj�Bj�Bk�BpB~YB��B�
B��B�_B��B��B�9B�|B�B�B��B�B�=BaB�B	�B�B�B�B�B�B�B�BBBB!"B"(B"$B#.B$5B#.B$2B%=B'JB+^B/xB:�BGBM*BSOBWdBYtB_�Bg�Bh�Bj�Bl�Bm�Bn�Bm�BrBx+B��B�yB�|B��B��B�_B��B��B�vB�qB��B�cB�B��Bo�BUeBRJBL.B5�B�B�B�BۆB�OBۈB�B�BܑB�UB�)B��B��B�qB�B��B�}Bl�BPOB>�B3�B$CBB�B�B
�XB
�BB
�B
�B
�SB
��B
�kB
�4B
��B
��B
��B
�yB
�tB
mB
}fB
zUB
mB
Y�B
H)B
;�B
6�B
3�B
,�B
�B	�wB	�B	ܪB	�,B	��B	��B	�>B	��B	uDB	f�B	X�B	PgB	H6B	@	B	:�B	3�B	,�B	=B	�B��B�_B�IB�!B��B��BܶB٫B֒BԉB�tB�iB�bB�^B�_B�WB�RB�NB�@B�4B�#B�B�!B�5B�6B�2B�2B�0B�&B�)B� B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�xB�pB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�]B�zB�iB�B��B�|B�IB�B��B��B�B~�B~�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�*B�+B�/B�,B�(B�4B�;B�<B�EB�FB�@B�AB�AB�\B�eB�kB�jB�_B�dB�NB�HB�eB�vB�}B��B��B��B��B��B��B�:B�nB�oB�}BՔBٮB��B��B��B��B�B�&B�BB�WB�mB��B	�B	�B	
�B	�B	�B	�B	B	 B	(B	0B	4B	OB	LB	6B	B	B	+B	"B	#B	#B	(B	AB	 SB	 TB	$kB	(�B	+�B	-�B	0�B	2�B	5�B	5�B	6�B	8�B	9�B	:�B	;�B	@B	C#B	E.B	F5B	G<B	HCB	IJB	IIB	IKB	JLB	JKB	ILB	M`B	Q|B	R�B	S�B	V�B	Z�B	^�B	d�B	g�B	kB	n$B	o*B	r>B	tHB	zmB	��B	��B	��B	��B	��B	��B	�B	�$B	�2B	�7B	�7B	�>B	�SB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�B	�B	�B	�B	�,B	�1B	�=B	�dB	�gB	�uB	�wB	ՍB	؜B	٨B	۴B	ܵB	ܵB	ܵB	ܶB	ݼB	ݻB	ݿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�$B	�%B	�.B	�@B	�FB	�HB	�XB	�NB	�YB	�QB	�ZB	�dB	�fB	�fG�O�B	�VB

�B
�B
SB
)�B
1�B
8BB
=DB
C�B
IBB
OKB
VAB
\B
_�B
foB
h/B
l�B
p_B
s B
wRB
yb1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.006(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940252019060409402520190604094025  AO  ARCAADJP                                                                    20181121041155    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041155  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041155  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094025  IP                  G�O�G�O�G�O�                