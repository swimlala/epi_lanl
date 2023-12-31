CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-06-04T07:01:51Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20180604070151  20190604094146  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�g�Oa�1   @�g���@6�$�/�d� ě��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy>�D��D�VD�|)D��=D��D�S3D�}�D��
D�RD�L{D���D��D�	�D�<)D�s3D���D��D�;3D�l)D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�@�A�HA@z�A^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D,�D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT�DUuDU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dtn�Dy:=D��D�S�D�y�D�� D���D�P�D�{�D���D�D�J>D���D���D��D�9�D�p�D࿯D��]D�8�D�i�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aƕ�AƑhAƓuAƉ7AƃA�t�A�ZA��A�`BA�?}A�5?A�+A�&�A�$�A�A�A�9XAĝ�A�(�A�9XA�S�A�M�A�A�p�A�x�A��A�  A��DA��A�&�A�|�A��mA�G�A��A�A�%A���A���A�
=A�  A��-A�
=A�XA��!A��RA�?}A�ȴA�dZA�ȴA�33A�-A��RA��
A���A��yA�1'A��A��A���A�(�A�x�A�VA���A���A�E�A�hsA��A��RA��A�oA��A��-A�G�A��\A��uA�dZA��A�5?A�x�A�x�A�%A�bNA���A���A�"�A��7A���A�Q�A��7A���A� �A��wA��A�v�A��yA�G�A���A�?}A��A���A���A�{A~�A|�jA|�Az�AzVAxv�Av�At��Arr�Apr�Ao�;Aot�An�9Al�\Ai�FAh�+Af�Ad�uAb��Aa33A`jA^�A]�hA]x�A\��A[�FAZ��AXffAW��AW33AU�TAU�AT��AS��AQS�APȴAP9XAO�AOt�AN��AM�;AL1AI��AHȴAG�AF�HAF�AE|�ADn�AC��AC&�AA"�A@E�A?K�A<1'A;hsA:�A:bA9��A9��A97LA8�A8��A7�^A6v�A5�A5oA4{A2�`A21'A1l�A1�A0n�A/�^A.�/A-C�A*�HA)��A)��A(�9A(-A&�A%�A#�#A �A�A��A�A�\A��AdZAC�AoA�RAM�A�A�
A"�AA�PA+A��AG�A��AbNA1AAXA�RA�-A/A�mA��A�-A33A
VA
�A	%AK�A��A�AQ�A��Al�A&�A �HA ��A �\@�;d@��9@��@�V@�X@���@�@��#@���@��m@�n�@�Ĝ@�ƨ@ꗍ@陚@���@��@���@�@�+@�h@�@�-@��@�t�@�=q@�X@���@�1'@ۍP@�ff@١�@ؼj@���@ӍP@�V@�%@϶F@�v�@͙�@�%@�9X@�;d@ʸR@�7L@ȼj@�|�@�ff@��@őh@���@�Ĝ@���@�o@�n�@�x�@��9@�Z@��@��
@�33@��y@���@�V@�5?@���@��7@�X@��/@�A�@��w@���@�5?@�p�@�I�@��
@�K�@�33@�@��y@��@���@�-@��-@��-@�7L@�9X@���@�dZ@�;d@�o@�@��y@���@���@��\@�^5@��@��@��/@��@���@�I�@���@�|�@�33@��@��H@���@�ff@���@��7@���@��j@��/@���@��D@�ȴ@�^5@�J@���@�Q�@�1'@�b@��@�C�@�^5@���@���@�9X@�  @���@�;d@�+@�-@��#@���@�V@��@�(�@��@�\)@��@��@��@���@��y@�~�@�M�@�-@��@���@��^@�x�@�?}@�V@��@�bN@��@���@�"�@���@��\@�n�@�{@�p�@�hs@�`B@�G�@�/@��`@���@��D@��m@���@�33@��H@�{@�`B@��h@���@���@��h@��7@��@�x�@��@���@�z�@�A�@��@� �@�b@� �@��
@��y@�n�@�^5@�V@�E�@�@���@��-@���@��^@���@��T@���@�x�@�G�@���@�Ĝ@��j@�Ĝ@��D@�Q�@�Q�@�Q�@�Q�@�Q�@�1'@��m@��@��P@�|�@�l�@�C�@���@��\@��@��@��#@���@�X@���@���@���@�1'@���@��w@�S�@���@��@���@��@��@�ȴ@���@�^5@�J@��T@���@���@�hs@�%@���@���@��@�bN@��[@�p;@�@u��@ol�@d�	@]�@V�h@N\�@Fȴ@@h�@;C�@4��@.e@(4n@!��@��@g�@��@�.@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aƕ�AƑhAƓuAƉ7AƃA�t�A�ZA��A�`BA�?}A�5?A�+A�&�A�$�A�A�A�9XAĝ�A�(�A�9XA�S�A�M�A�A�p�A�x�A��A�  A��DA��A�&�A�|�A��mA�G�A��A�A�%A���A���A�
=A�  A��-A�
=A�XA��!A��RA�?}A�ȴA�dZA�ȴA�33A�-A��RA��
A���A��yA�1'A��A��A���A�(�A�x�A�VA���A���A�E�A�hsA��A��RA��A�oA��A��-A�G�A��\A��uA�dZA��A�5?A�x�A�x�A�%A�bNA���A���A�"�A��7A���A�Q�A��7A���A� �A��wA��A�v�A��yA�G�A���A�?}A��A���A���A�{A~�A|�jA|�Az�AzVAxv�Av�At��Arr�Apr�Ao�;Aot�An�9Al�\Ai�FAh�+Af�Ad�uAb��Aa33A`jA^�A]�hA]x�A\��A[�FAZ��AXffAW��AW33AU�TAU�AT��AS��AQS�APȴAP9XAO�AOt�AN��AM�;AL1AI��AHȴAG�AF�HAF�AE|�ADn�AC��AC&�AA"�A@E�A?K�A<1'A;hsA:�A:bA9��A9��A97LA8�A8��A7�^A6v�A5�A5oA4{A2�`A21'A1l�A1�A0n�A/�^A.�/A-C�A*�HA)��A)��A(�9A(-A&�A%�A#�#A �A�A��A�A�\A��AdZAC�AoA�RAM�A�A�
A"�AA�PA+A��AG�A��AbNA1AAXA�RA�-A/A�mA��A�-A33A
VA
�A	%AK�A��A�AQ�A��Al�A&�A �HA ��A �\@�;d@��9@��@�V@�X@���@�@��#@���@��m@�n�@�Ĝ@�ƨ@ꗍ@陚@���@��@���@�@�+@�h@�@�-@��@�t�@�=q@�X@���@�1'@ۍP@�ff@١�@ؼj@���@ӍP@�V@�%@϶F@�v�@͙�@�%@�9X@�;d@ʸR@�7L@ȼj@�|�@�ff@��@őh@���@�Ĝ@���@�o@�n�@�x�@��9@�Z@��@��
@�33@��y@���@�V@�5?@���@��7@�X@��/@�A�@��w@���@�5?@�p�@�I�@��
@�K�@�33@�@��y@��@���@�-@��-@��-@�7L@�9X@���@�dZ@�;d@�o@�@��y@���@���@��\@�^5@��@��@��/@��@���@�I�@���@�|�@�33@��@��H@���@�ff@���@��7@���@��j@��/@���@��D@�ȴ@�^5@�J@���@�Q�@�1'@�b@��@�C�@�^5@���@���@�9X@�  @���@�;d@�+@�-@��#@���@�V@��@�(�@��@�\)@��@��@��@���@��y@�~�@�M�@�-@��@���@��^@�x�@�?}@�V@��@�bN@��@���@�"�@���@��\@�n�@�{@�p�@�hs@�`B@�G�@�/@��`@���@��D@��m@���@�33@��H@�{@�`B@��h@���@���@��h@��7@��@�x�@��@���@�z�@�A�@��@� �@�b@� �@��
@��y@�n�@�^5@�V@�E�@�@���@��-@���@��^@���@��T@���@�x�@�G�@���@�Ĝ@��j@�Ĝ@��D@�Q�@�Q�@�Q�@�Q�@�Q�@�1'@��m@��@��P@�|�@�l�@�C�@���@��\@��@��@��#@���@�X@���@���@���@�1'@���@��w@�S�@���@��@���@��@��@�ȴ@���@�^5@�J@��T@���@���@�hs@�%@���@���@��G�O�@��[@�p;@�@u��@ol�@d�	@]�@V�h@N\�@Fȴ@@h�@;C�@4��@.e@(4n@!��@��@g�@��@�.@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
 �B
 �B
 �B
"�B
$�B
'�B
,B
`BB
�B7LB>wBD�BJ�B`BBm�B|�B��BɺB�TB��BPB)�B33BE�BT�B`BBhsBr�B{�B�B�DB�hB��B��B��B��B��B��B��B��B��B��B��B�oB�JB�DB��B��B��B�VB�7B�B~�By�Bv�Bv�Bu�Br�Bk�B\)BI�BF�BB�B=qB5?B0!B.B$�B�BPB%BB��B�B�B�NB��B�qB�FB��B�BdZBR�BH�B:^B"�B
��B
��B
�B
ƨB
�B
��B
�bB
�VB
�JB
�+B
}�B
m�B
dZB
^5B
ZB
S�B
Q�B
T�B
T�B
R�B
E�B
7LB
,B
�B
hB
PB
	7B
B	�B	�NB	�B	��B	�}B	�3B	��B	��B	��B	�oB	�hB	�JB	�B	|�B	p�B	k�B	gmB	_;B	ZB	W
B	Q�B	F�B	A�B	>wB	;dB	8RB	2-B	,B	!�B	�B	bB	VB	
=B	%B	B��B��B�B�sB�NB�/B�
B�B��B��B��B��B��B��B��BǮBŢBĜBB�}B�^B�FB�9B�3B�-B�B�B��B��B��B��B��B�oB�\B�JB�+B�B�B�B�B�B�B�B�B� B� B~�B}�B|�B{�B{�Bz�By�By�Bw�Bw�Bv�Bu�Bt�Bq�Bn�BjBhsBbNB_;B\)BZBW
BS�BQ�BM�BL�BN�BM�BN�BN�BO�BO�BN�BM�BM�BK�BJ�BM�BL�BJ�BJ�BK�BK�BJ�BJ�BJ�BJ�BK�BK�BK�BJ�BJ�BL�BN�BQ�BR�BR�BQ�BQ�BR�BR�BR�BS�BS�BT�BT�BW
B\)B^5B`BBbNBdZBffBhsBhsBjBl�Bl�Bp�Bq�Bt�Bx�Bz�B|�B� B�B�B�%B�1B�DB�\B�bB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�?B�?B�FB�FB�FB�LB�XB�jB�jB�}BŢB��B��B��B��B��B��B��B��B��B��B�B�B�)B�5B�5B�5B�5B�;B�NB�fB�B�B�B��B��B��B	B	1B		7B	JB	�B	�B	�B	(�B	,B	-B	.B	2-B	5?B	:^B	?}B	C�B	I�B	K�B	N�B	R�B	W
B	`BB	cTB	dZB	hsB	m�B	p�B	p�B	v�B	y�B	y�B	y�B	x�B	x�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�JB	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�?B	�?B	�FB	�RB	�^B	�dB	�qB	�}B	��B	�}B	�}B	��B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�BB	�TB	�TB	�TB	�TB	�TB	�TB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
 4B
�B
�B
(�B
1AB
3B
AoB
C-B
L�B
QhB
W$B
YB
^B
`�B
c�B
f�B
l�B
q�B
xB
|�B
�G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
B
B
B
B
B
1B
FB
O|B
٦B&fB-�B3�B9�BO]B\�BlB��B��B�ZB��B�QB�B"0B4�BC�BO;BWnBa�Bj�BsBz8B�_B�vB��B��B��B��B��B��B��B��B��B��B�fB{CBz<B��B��B�{B}MBx2Bq
Bm�Bh�Be�Be�Bd�Ba�BZ�BK(B8�B5�B1�B,zB$FB%BB�B�B�^B�0B�B��B��BڙB�cB��B��B�aB��Bs9BS~BBB7�B)�BB
�!B
�,B
��B
��B
�[B
�B
�B
}�B
{�B
vxB
m=B
\�B
S�B
M�B
IlB
CJB
A:B
DMB
DQB
B?B
4�B
&�B
\B
B
 �B	��B	��B	�nB	�B	ѮB	�|B	�BB	��B	��B	�kB	�?B	�B	��B	��B	{�B	t�B	l`B	`B	Z�B	V�B	N�B	I�B	F�B	AhB	6%B	1B	-�B	*�B	'�B	!�B	�B	LB	B��B��B��B��B�B�fB�EB�+B��B��B̺BƕBŏBĉB�|B�zB�oB�cB�\B�QB�<B�3B�-B�!B�B��B��B��B��B��B��B��B��B�UB�EB�:B�B�B~�B{�Bv�Br�Br�Bq�Bq�Bp�Bq�Bq�Bp�Bo�Bo�Bn�Bm�Bl�Bk�Bk�Bj~Bi|Bi{BgqBgqBflBegBd`BaQB^;BZBXBQ�BN�BK�BI�BF�BC�BA�B=|B<yB>�B=�B>�B>�B?�B?�B>�B=�B=~B;uB:pB=�B<yB:nB:lB;rB;uB:nB:pB:iB:nB;vB;tB;sB:pB:nB<|B>�BA�BB�BB�BA�BA�BB�BB�BB�BC�BC�BD�BD�BF�BK�BM�BO�BQ�BT	BVBXBXBZ+B\9B\5B`QBaUBdgBh�Bj�Bl�Bo�Bp�Bs�Bu�Bw�Bz�BB�B�B�B�-B�1B�7B�FB�DB�HB�NB�UB�ZB�jB�mB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�IB�iB�iB�gB�hB�kB�sB�wB�~B��B��BǴBɿB��B��B��B��B��B��B��B�
B�!B�3B�<B�B�xB�xB��B��B��B��B		4B	FB	OB	�B	�B	�B	�B	!�B	$�B	)�B	/B	3+B	9QB	;aB	>oB	B�B	F�B	O�B	R�B	S�B	XB	]#B	`4B	`4B	fZB	ikB	ikB	iiB	hgB	hgB	l~B	n�B	o�B	o�B	p�B	q�B	s�B	t�B	u�B	w�B	y�B	z�B	{�B	~�B	��B	� B	��B	�	B	�*B	�(B	�*B	�+B	�+B	�2B	�3B	�:B	�VB	�]B	�kB	�cB	�YB	�eB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�&B	�0B	�:B	�IB	�RB	�aB	ĄB	ŊB	ǛB	ȟB	ʩB	˰B	˯B	ˮB	̳B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�$B	�.B	�6B	�;B	�AB	�@B	�CB	�HB	�IB	�LB	�XB	�gB	�mB	�mB	�mB	�kB	�mB	�iB	�tB	�{B	�B	��B	��B	�B	�B	�B	�B	��G�O�B	�B	�HB
bB
\B
 �B
"�B
0�B
2�B
<B
@�B
F�B
H�B
M�B
PIB
SnB
VhB
\UB
a%B
g�B
lB
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941462019060409414620190604094146  AO  ARCAADJP                                                                    20180604070151    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180604070151  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180604070151  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094146  IP                  G�O�G�O�G�O�                