CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:56Z creation      
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
_FillValue                 �  ]t   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g`   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qL   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181121041156  20190604094026  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��:����1   @��;k$%5@4C��$��d�^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� DafDa� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyj=D��D�I�D��=D�� D�qD�`�D���D���D��D�:�D���D��)D�
D�6�DڎD��)D���D�9HD�pR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH��DI�DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_uD_��D`{�Da�Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dye�D��D�G]D�� D���D�4D�^gD��RD���D��RD�8RD���D���D��D�4{Dڋ�D���D��qD�7D�n1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��RA�n�A�I�A�7LA�/A�$�A��A��A�%A���A���A��A��A��A��yA��mA��HA��A�O�A�I�A��A���A��9A���A���A��DA�~�A�p�A�p�A�p�A�n�A�n�A�jA�^5A�Q�A�E�A�9XA�(�A���A��A��A���A��A�~�A�`BA�XA�&�A�A��mA���A�&�A�C�A���A�9XA���A���A�n�A�
=A�dZA�A�\)A�x�A���A�n�A�JA���A��A�M�A���A���A��9A�ffA� �A��^A�;dA���A�/A�{A�1A��A�K�A�x�A���A�JA�K�A��mA���A�ZA��HA���A���A��TA��wA� �A�p�A��\A�z�A�XA��;A��;A�%A��A�ȴA�ZA��wA�Q�A�r�A��PA�-A��;A�oA���A��A��A��-A�z�A�=qA���A���A�I�A~��A{��Ay/Aw��Aw+Av-AuoAtr�Ar1'Aq�hApAl��Akx�Aj1Ah��Ag;dAe7LAbn�Aa?}A_�PA^��A\�HA\jA[�AW��AUhsATE�AR�jAQ
=AOO�AMO�AK�AJ��AH��AE�PAD�yAD(�AC�AC��AC7LAB�jAB�DA@�A>M�A>A=��A<�+A<bA;?}A:��A9�A8�A733A4��A4JA2�`A1�-A0�yA0JA/�A.VA-33A,�A+�A*jA)��A(v�A&�RA$�9A#�hA"��A"ZA!��A!�A �`A �A�DA�!A�A`BAĜAZA�A�A�^Ax�AK�A�TAn�A�AS�Ar�A`BA\)AA�AXAAO�AA
��A	��A	oAr�A�AAC�A��A�7A�A`BAC�AVAjAA�;A��A�A��A�A��AbNA��A �@��@�1'@���@��@�X@��F@�$�@�C�@�G�@��@��@�9@�+@�bN@���@��@�^@�V@ߕ�@ܴ9@���@���@ى7@��y@�{@Ցh@�X@��`@�b@��m@ӶF@ӍP@�33@�o@ҟ�@���@�bN@���@�n�@�E�@���@��H@ɩ�@�bN@�|�@�
=@�ȴ@Ɵ�@�n�@�=q@���@���@��@�K�@��@��@�j@�b@���@���@�V@���@��@�7L@�X@�hs@�x�@�/@�
=@�?}@���@��@���@��u@�I�@��m@��F@���@�33@���@�-@��D@�S�@���@�~�@�^5@�=q@��@���@���@��-@�X@�V@��@�/@�%@��D@��@���@���@�hs@���@��@� �@�|�@�
=@���@��@��-@��-@�hs@���@��@�33@��y@��!@�E�@�?}@�9X@�b@��@�l�@�;d@�o@��R@�M�@�5?@�{@��@��#@��-@��7@�hs@�V@��/@���@��j@�r�@�9X@�  @��P@�\)@��@���@�v�@�^5@�E�@�M�@�M�@�5?@�$�@�$�@��@�J@���@�O�@�&�@�V@�z�@�r�@���@�X@���@��@�O�@��@�Ĝ@���@��D@�z�@�bN@�Q�@�9X@��@���@���@�t�@�33@�
=@���@�n�@�E�@��-@��7@��T@�J@��-@�X@�&�@���@��9@�bN@��m@���@��P@�"�@���@�~�@�-@��^@���@���@��h@��h@���@�?}@��@�V@��@���@��@�z�@�Q�@�  @���@��w@��F@��@���@���@���@���@��@��@��@�dZ@��@�M�@�@��#@���@��7@�hs@�`B@�7L@���@��@���@�t�@���@��@���@�&�@�Ĝ@��@�-w@�n/@yF@o�A@f5?@`[�@W(@RO@K�@GS@A�@:�@0�@)s�@%s�@˒@��@��@�@c 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A��RA�n�A�I�A�7LA�/A�$�A��A��A�%A���A���A��A��A��A��yA��mA��HA��A�O�A�I�A��A���A��9A���A���A��DA�~�A�p�A�p�A�p�A�n�A�n�A�jA�^5A�Q�A�E�A�9XA�(�A���A��A��A���A��A�~�A�`BA�XA�&�A�A��mA���A�&�A�C�A���A�9XA���A���A�n�A�
=A�dZA�A�\)A�x�A���A�n�A�JA���A��A�M�A���A���A��9A�ffA� �A��^A�;dA���A�/A�{A�1A��A�K�A�x�A���A�JA�K�A��mA���A�ZA��HA���A���A��TA��wA� �A�p�A��\A�z�A�XA��;A��;A�%A��A�ȴA�ZA��wA�Q�A�r�A��PA�-A��;A�oA���A��A��A��-A�z�A�=qA���A���A�I�A~��A{��Ay/Aw��Aw+Av-AuoAtr�Ar1'Aq�hApAl��Akx�Aj1Ah��Ag;dAe7LAbn�Aa?}A_�PA^��A\�HA\jA[�AW��AUhsATE�AR�jAQ
=AOO�AMO�AK�AJ��AH��AE�PAD�yAD(�AC�AC��AC7LAB�jAB�DA@�A>M�A>A=��A<�+A<bA;?}A:��A9�A8�A733A4��A4JA2�`A1�-A0�yA0JA/�A.VA-33A,�A+�A*jA)��A(v�A&�RA$�9A#�hA"��A"ZA!��A!�A �`A �A�DA�!A�A`BAĜAZA�A�A�^Ax�AK�A�TAn�A�AS�Ar�A`BA\)AA�AXAAO�AA
��A	��A	oAr�A�AAC�A��A�7A�A`BAC�AVAjAA�;A��A�A��A�A��AbNA��A �@��@�1'@���@��@�X@��F@�$�@�C�@�G�@��@��@�9@�+@�bN@���@��@�^@�V@ߕ�@ܴ9@���@���@ى7@��y@�{@Ցh@�X@��`@�b@��m@ӶF@ӍP@�33@�o@ҟ�@���@�bN@���@�n�@�E�@���@��H@ɩ�@�bN@�|�@�
=@�ȴ@Ɵ�@�n�@�=q@���@���@��@�K�@��@��@�j@�b@���@���@�V@���@��@�7L@�X@�hs@�x�@�/@�
=@�?}@���@��@���@��u@�I�@��m@��F@���@�33@���@�-@��D@�S�@���@�~�@�^5@�=q@��@���@���@��-@�X@�V@��@�/@�%@��D@��@���@���@�hs@���@��@� �@�|�@�
=@���@��@��-@��-@�hs@���@��@�33@��y@��!@�E�@�?}@�9X@�b@��@�l�@�;d@�o@��R@�M�@�5?@�{@��@��#@��-@��7@�hs@�V@��/@���@��j@�r�@�9X@�  @��P@�\)@��@���@�v�@�^5@�E�@�M�@�M�@�5?@�$�@�$�@��@�J@���@�O�@�&�@�V@�z�@�r�@���@�X@���@��@�O�@��@�Ĝ@���@��D@�z�@�bN@�Q�@�9X@��@���@���@�t�@�33@�
=@���@�n�@�E�@��-@��7@��T@�J@��-@�X@�&�@���@��9@�bN@��m@���@��P@�"�@���@�~�@�-@��^@���@���@��h@��h@���@�?}@��@�V@��@���@��@�z�@�Q�@�  @���@��w@��F@��@���@���@���@���@��@��@��@�dZ@��@�M�@�@��#@���@��7@�hs@�`B@�7L@���@��@���@�t�@���@��@���@�&�@�ĜG�O�@�-w@�n/@yF@o�A@f5?@`[�@W(@RO@K�@GS@A�@:�@0�@)s�@%s�@˒@��@��@�@c 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBhsBhsBhsBiyBiyBiyBjBjBk�Bl�Bm�Bn�Bo�Bo�Bo�Bp�Bq�Br�B�+B��B�'B�3B�9B�?B�FB�LB�LB�LB�LB�RB�RB�RB�XB�XB�XB�^B�^B�dB�}B�}B�}BBĜBǮBȴBǮBɺB��BɺB��B��B��B�`B�yB�B�yB�sB�fB�`B�TB�HB�#B�
B�B�B�
B��B��B��BǮB�}B�XB�!B��B��B�PB�Bu�BS�B<jB(�B�BPBB��B��B��B��B�B�B�BB�B��BǮB��B��B�PB{�BhsBZBM�BD�BA�B;dB33B-B �BuBB
�sB
�BB
�B
ǮB
�FB
�B
�B
��B
��B
��B
�bB
�B
e`B
R�B
I�B
C�B
>wB
7LB
1'B
#�B
�B
PB	��B	�fB	�B	��B	ÖB	�FB	��B	��B	�\B	�7B	�B	|�B	s�B	ffB	\)B	VB	O�B	I�B	B�B	5?B	.B	+B	�B	�B	�B	�B	�B	�B	�B	uB	bB		7B��B��B��B�B�B�B�sB�ZB�BB�B��B��B��BɺBƨBÖB��B��B�}B�qB�jB�^B�LB�3B�B�B��B��B��B��B��B��B��B��B�{B�hB�VB�PB�JB�DB�DB�=B�7B�1B�B�B~�B|�Bz�Bw�Bu�Bt�By�B~�B�7B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�RB�^BĜBƨBǮBƨBƨBŢBÖB�wB�dB�RB�9B�!B��B��B��B��B�B�B�B�B�B�B�B�!B�-B�-B�3B�?B�?B�FB�LB�RB�XB�dB�qB��BƨBŢBŢBƨBȴBɺBȴBɺB��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�#B�fB�sB�B�B�B�B�B�B��B	B	DB	JB	PB	PB	VB	\B	\B	\B	bB	hB	oB	�B	�B	"�B	$�B	%�B	&�B	&�B	'�B	'�B	(�B	+B	/B	2-B	33B	5?B	7LB	;dB	>wB	B�B	B�B	D�B	J�B	K�B	K�B	K�B	L�B	L�B	M�B	M�B	L�B	L�B	N�B	P�B	Q�B	Q�B	R�B	W
B	ZB	ZB	[#B	]/B	]/B	^5B	`BB	bNB	cTB	dZB	e`B	ffB	gmB	gmB	hsB	iyB	jB	jB	k�B	l�B	m�B	n�B	s�B	u�B	z�B	~�B	� B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	�B	�'B	�RB	�^B	�qB	�}B	�}B	��B	��B	B	B	ÖB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�/B	�5B	�;B	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
�B
YB
$�B
+6B
2�B
:�B
>BB
C{B
JrB
PB
TB
ZB
a�B
f�B
jB
o5B
raB
u�B
{dB
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111Ba�Ba�Ba�Bb�Bb�Bb�Bc�Bc�Bd�Be�Bf�Bg�Bh�Bh�Bh�Bi�Bj�Bk�B�wB�@B�oB�zB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�:BުB�B��B�B�B߰BޫBܙBڐB�lB�VB�]B�QB�XB�EB�8B�!B��B��B��B�gB�8B�	B��B}kBoBMFB5�B"JBB�B�oB�>B�*B�B�B��B��BٚB�kB�NB�B�LB�B��BuEBa�BSBG7B=�B:�B4�B,�B&rB%B�B
�oB
��B
٬B
ӅB
�B
��B
�|B
�jB
�LB
�B
��B
��B
|�B
^�B
LcB
C(B
=B
7�B
0�B
*�B
GB
B
�B	�4B	��B	ӗB	�SB	�B	��B	�KB	�B	��B	��B	z�B	vmB	m2B	_�B	U�B	O�B	I^B	C:B	<B	.�B	'�B	$�B	?B	$B	 B	B	B	B	B	�B		�B	�B�sB�gB�UB�7B�#B�B��B��B��BҟB�}B�hB�YB�BB�1B�B�B�B�B��B��B��B��B��B��B��B�yB�vB�kB�^B�VB�LB�9B�!B�B��B��B��B��B��B��B��B��B��B|�Bz�Bx�Bv�BtsBq`BoSBnMBskBx�B��B��B�6B�[B�kB�qB�zB�oB�eB�sB�lB�lB�lB�oB�hB�pB�uB�uB�yB��B��B��B��B��B��B��B��B�+B�8B�<B�:B�9B�2B�#B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�6B�3B�0B�6B�GB�JB�CB�IB�UB�[B�^B�]B�aB�_B�fB�mB�nB�|BΎBϖBМBѢBԴB��B�B�B�B�$B�'B�-B�>B�kB��B	�B	�B	�B	�B	�B	�B	�B	�B		�B	
�B	�B	$B	LB	[B	jB	tB	 wB	 uB	!}B	!~B	"�B	$�B	(�B	+�B	,�B	.�B	0�B	4�B	8B	<B	<B	>*B	DKB	ETB	ETB	ERB	FUB	FWB	G]B	G^B	FZB	F\B	HfB	JpB	KzB	KyB	L}B	P�B	S�B	S�B	T�B	V�B	V�B	W�B	Y�B	[�B	\�B	]�B	^�B	_�B	`�B	`�B	a�B	cB	d	B	dB	eB	fB	gB	h$B	mEB	oLB	tnB	x�B	y�B	z�B	{�B	|�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�DB	�iB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�.B	�KB	�XB	�bB	�cB	�mB	�lB	�yB	́B	БB	ҟB	ծB	ֲB	׻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�(B	�/B	�0B	�1B	�4B	�5B	�5B	�1B	�6B	�<B	�SB	�VB	�_B	�eB	�nB	�rB	�|G�O�B	�tB
�B
FB
$�B
,7B
4KB
7�B
<�B
C�B
I�B
M�B
S�B
[hB
`B
c�B
h�B
k�B
o+B
t�B
x,1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.006(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940262019060409402620190604094026  AO  ARCAADJP                                                                    20181121041156    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041156  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041156  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094026  IP                  G�O�G�O�G�O�                