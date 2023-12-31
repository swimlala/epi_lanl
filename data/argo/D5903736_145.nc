CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-15T19:16:34Z AOML 3.0 creation; 2016-05-31T19:14:48Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7$   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7(   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7,   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7<   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7L   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7\   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7d   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8    DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8@   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8h   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
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
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160415191634  20190604094001  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_145                   2C  D   APEX                            5368                            041511                          846 @פ����1   @פ�_�Zr@3��-�d3"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dys3D�3D�C3D�y�D���D��D�@ D�� D�� D��D�0 D��3DǶfD�3D�33Dڐ D��3D�fD�)�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @5�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBx�B�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�zC_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt[�Dyn�D��D�@�D�w]D�ʐD�
�D�=�D���D���D�]D�-�D���DǴ)D� �D�0�Dڍ�D���D�)D�']D�}�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��`A��yA��yA��A��yA���Aʙ�AʍPAʉ7AʃA�v�A�l�A�(�A�9XA�G�A�ƨAƏ\A�ZA��A���A���A�z�A�hsA�M�A�G�A�7LA��HA�`BAã�A� �A©�AA�S�A�&�A���A�t�A���A���A���A���A�M�A�C�A�1'A�JA���A��HA��^A��DA��A��9A�\)A��A�1A�\)A�"�A��A���A��/A�|�A�`BA���A�|�A��;A��A�A��HA�n�A�ZA�l�A�n�A� �A�v�A�JA���A���A�7LA�A�A���A���A�-A�O�A�x�A�p�A���A�=qA�A��A�Q�A�A�&�A�7LA��mA��!A��;A�~�A�ƨA�~�A���A�A�A��A��yA�7LA�7LA��A��jA��
A�bNA��^A�%A���A���A�\)A��7A���A��A�7A&�A~n�A}�A}dZA|=qAzffAwO�AuO�Ar1Ak�;Aj��Aj�Ag�wAe��AdE�Ac&�AaO�A_��A]��AZA�AX��AU`BAS�;ARn�APjAM��AI��AG�AD~�AC"�AA�A@jA>�yA=�A;C�A:z�A9�
A8bA5A4�A3oA2VA1dZA0��A0ffA-p�A+�A*A�A(A&�yA&-A%
=A"��A!XA A�AƨA�AXA �A�uA��A�yA�Ar�Ap�A�+A��AS�AVA5?A��AI�AdZAA�\A=qA�A�#A�/A;dA
�A	�FA	�A�RA��A�9A��A�A�A�A�AM�A1'A�yA?}A V@�5?@�&�@���@��@� �@�V@�K�@�E�@��@�b@�C�@�p�@��m@�@�r�@��@�@�x�@㝲@�ff@���@�j@���@�^5@݁@�7L@ۍP@�x�@��@�{@��/@��m@���@�5?@҇+@���@љ�@��@�J@Ɂ@ȴ9@�Ĝ@�O�@ȼj@���@���@�p�@�V@���@Ĵ9@���@�K�@�ȴ@�5?@�G�@�&�@��j@�Z@��@��P@�dZ@���@��R@�ff@��@�A�@��P@�"�@���@���@�ff@�@�x�@�`B@�G�@���@��w@���@�-@�p�@��u@�l�@�"�@���@�ȴ@���@��+@�v�@���@��-@���@�X@�%@���@��9@�9X@�33@���@��@�X@��j@��D@�r�@�A�@��;@��F@�\)@�+@�@���@�$�@�J@��@��T@���@��7@�O�@�?}@��@���@��9@��m@��P@�
=@��\@��\@�ff@�$�@�@�@�p�@�O�@�V@��`@��u@�r�@�Q�@�9X@��@���@�;d@��@��@�hs@�G�@�7L@��@�/@�7L@�V@��`@�9X@�7L@���@�\)@�dZ@�K�@���@�o@��@�7L@��`@�/@���@��@�V@���@���@��@�v�@�7L@�7L@�x�@���@�$�@��\@��!@���@�~�@��@�`B@��/@�z�@�z�@��
@�S�@�dZ@���@�33@��@���@��@�-@�@�-@�=q@�@�@��7@��@��/@�b@�
=@�n�@��H@�@��-@��h@�p�@�O�@�?}@�&�@��@��/@��/@�I�@��@�|�@�dZ@��y@�~�@�5?@���@��@�v�@��\@�ȴ@��\@�^5@�5?@��@��@��7@���@���@���@��/@�&�@�7L@�hs@�`B@�V@��9@��D@�Z@��@�K�@��@�
=@��R@��!@�n�@�@���@���@�`B@���@���@��D@�z�@�A�@���@��@�t�@�"�@��@��+@�^5@�5?@�=q@�V@�E�@�5?@���@���@��7@���@z�@up�@i�#@`�`@X�u@P��@G��@>ȴ@6@/��@*J@%�@ �9@��@|�@t�@�@z�@	�@\)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A��`A��yA��yA��A��yA���Aʙ�AʍPAʉ7AʃA�v�A�l�A�(�A�9XA�G�A�ƨAƏ\A�ZA��A���A���A�z�A�hsA�M�A�G�A�7LA��HA�`BAã�A� �A©�AA�S�A�&�A���A�t�A���A���A���A���A�M�A�C�A�1'A�JA���A��HA��^A��DA��A��9A�\)A��A�1A�\)A�"�A��A���A��/A�|�A�`BA���A�|�A��;A��A�A��HA�n�A�ZA�l�A�n�A� �A�v�A�JA���A���A�7LA�A�A���A���A�-A�O�A�x�A�p�A���A�=qA�A��A�Q�A�A�&�A�7LA��mA��!A��;A�~�A�ƨA�~�A���A�A�A��A��yA�7LA�7LA��A��jA��
A�bNA��^A�%A���A���A�\)A��7A���A��A�7A&�A~n�A}�A}dZA|=qAzffAwO�AuO�Ar1Ak�;Aj��Aj�Ag�wAe��AdE�Ac&�AaO�A_��A]��AZA�AX��AU`BAS�;ARn�APjAM��AI��AG�AD~�AC"�AA�A@jA>�yA=�A;C�A:z�A9�
A8bA5A4�A3oA2VA1dZA0��A0ffA-p�A+�A*A�A(A&�yA&-A%
=A"��A!XA A�AƨA�AXA �A�uA��A�yA�Ar�Ap�A�+A��AS�AVA5?A��AI�AdZAA�\A=qA�A�#A�/A;dA
�A	�FA	�A�RA��A�9A��A�A�A�A�AM�A1'A�yA?}A V@�5?@�&�@���@��@� �@�V@�K�@�E�@��@�b@�C�@�p�@��m@�@�r�@��@�@�x�@㝲@�ff@���@�j@���@�^5@݁@�7L@ۍP@�x�@��@�{@��/@��m@���@�5?@҇+@���@љ�@��@�J@Ɂ@ȴ9@�Ĝ@�O�@ȼj@���@���@�p�@�V@���@Ĵ9@���@�K�@�ȴ@�5?@�G�@�&�@��j@�Z@��@��P@�dZ@���@��R@�ff@��@�A�@��P@�"�@���@���@�ff@�@�x�@�`B@�G�@���@��w@���@�-@�p�@��u@�l�@�"�@���@�ȴ@���@��+@�v�@���@��-@���@�X@�%@���@��9@�9X@�33@���@��@�X@��j@��D@�r�@�A�@��;@��F@�\)@�+@�@���@�$�@�J@��@��T@���@��7@�O�@�?}@��@���@��9@��m@��P@�
=@��\@��\@�ff@�$�@�@�@�p�@�O�@�V@��`@��u@�r�@�Q�@�9X@��@���@�;d@��@��@�hs@�G�@�7L@��@�/@�7L@�V@��`@�9X@�7L@���@�\)@�dZ@�K�@���@�o@��@�7L@��`@�/@���@��@�V@���@���@��@�v�@�7L@�7L@�x�@���@�$�@��\@��!@���@�~�@��@�`B@��/@�z�@�z�@��
@�S�@�dZ@���@�33@��@���@��@�-@�@�-@�=q@�@�@��7@��@��/@�b@�
=@�n�@��H@�@��-@��h@�p�@�O�@�?}@�&�@��@��/@��/@�I�@��@�|�@�dZ@��y@�~�@�5?@���@��@�v�@��\@�ȴ@��\@�^5@�5?@��@��@��7@���@���@���@��/@�&�@�7L@�hs@�`B@�V@��9@��D@�Z@��@�K�@��@�
=@��R@��!@�n�@�@���@���@�`B@���@���@��D@�z�@�A�@���@��@�t�@�"�@��@��+@�^5@�5?@�=q@�V@�E�@�5?@���@���@��7@���@z�@up�@i�#@`�`@X�u@P��@G��@>ȴ@6@/��@*J@%�@ �9@��@|�@t�@�@z�@	�@\)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBaHBaHBaHBaHBbNBcTBhsBn�Bo�Bp�Bq�Bq�Br�B�B��B��B%BDBbBoB{B{B�B�B�B{B�B�B�B#�B'�B/BJ�B_;BdZBe`BiyBn�By�B{�B� B�B�B�B�B�B�+B�=B�JB��B��B��B��B�!B�^B�XB�LB��B�B�B�B��B��B��B�B�B�fB�
B�BB�HB��BǮB�}B�?B��B|�Bq�BdZB_;BW
B?}B0!B6FB;dB6FB2-B+B�B%B�B�B��B��B��B�}B�B��B��B��B�B�B��B�bBu�B_;BO�B2-B	7B
��B
�B
�TB
��B
��B
�jB
��B
�7B
�B
� B
y�B
t�B
n�B
cTB
Q�B
7LB
�B	��B	��B	�wB	�RB	��B	�uB	�=B	�B	y�B	o�B	aHB	K�B	?}B	-B	!�B	�B	PB	  B�B�ZB�/B�B��BȴB�wB�^B�^B�RB�FB�3B�B��B��B��B��B��B�uB�oB�bB�\B�PB�7B�%B�B�B~�B�B�B�B� B� B� B~�B~�B~�B}�B{�Bx�Bv�Bs�Bq�Bp�Bn�Bl�Bk�Bm�Bm�Bn�Bp�Bt�Br�Bp�Bn�Bn�Bo�Bo�Bs�Bu�Bx�B� B�1B�B�B}�B|�By�Bv�Bs�Bs�Bw�Bw�Bw�Bv�Bv�Bw�Bw�Bw�Bw�Bv�Bu�Bu�Bs�Bq�Bn�Bl�Bl�Bo�Bp�Bo�Bq�Bt�Bu�Bw�Bx�Bz�Bz�Bz�B{�B|�B~�B�B�B�VB��B��B��B��B�bB�oB��B�B�'B�?B�RB�XB�^B�dB�dB�qB�}B��BÖBǮBǮBȴBɺB��B��B��B��B��B��B��B��B�
B�B�#B�#B�)B�BB�TB�`B�`B�mB�B�B�B�B��B	  B	B	B	%B	1B	
=B	JB	uB	�B	�B	�B	�B	�B	�B	#�B	&�B	%�B	%�B	)�B	/B	2-B	33B	6FB	9XB	;dB	@�B	B�B	D�B	G�B	L�B	M�B	N�B	O�B	O�B	Q�B	S�B	S�B	T�B	W
B	W
B	W
B	YB	]/B	bNB	cTB	dZB	ffB	gmB	hsB	iyB	iyB	k�B	m�B	t�B	v�B	w�B	x�B	x�B	y�B	{�B	|�B	|�B	}�B	� B	�B	�B	�+B	�VB	�\B	�VB	�DB	�B	~�B	� B	�B	�B	�1B	�\B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�9B	�dB	�}B	��B	��B	ÖB	ĜB	ǮB	ǮB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�;B	�BB	�;B	�;B	�;B	�/B	�)B	�BB	�/B	�)B	�/B	�HB	�`B	�`B	�ZB	�ZB	�fB	�yB	�B	�B	�yB	�B	�yB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
	7B
PB
�B
 �B
(�B
0!B
7LB
=qB
C�B
L�B
S�B
[#B
bNB
gmB
jB
o�B
t�B
w�B
z�B
~�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Ba�Ba�Ba�Ba�Bb�Bc�Bh�Bn�BpBqBrBrBsB�qB�cB�2B�B�B�B�B�B�B�B�B�B�B�BB )B$CB(ZB/�BK+B_�Bd�Be�Bi�BoBzIB|QB�iB��B�xB�zB��B��B��B��B��B��B��B�B�6B��B��B��B��B��B��B�B�B�/B�HB�/B�B�B��B�wB�B�B�kB�B��B��B�B}WBrBd�B_�BWyB?�B0�B6�B;�B6�B2�B+mBB�B�!B�|B�PB�CB�5B��B�lB�!B�B��B�gB��B�&B��Bv,B_�BPJB2�B	�B
�YB
�B
��B
�YB
�-B
��B
�0B
��B
�yB
�iB
z@B
u)B
oB
c�B
RXB
7�B
 &B	�OB	�2B	��B	��B	�LB	��B	��B	�|B	zCB	pB	a�B	L0B	?�B	-vB	"6B	�B	�B	 jB�B�BݙB�yB�KB�B��B��B��B��B��B��B�yB�VB�8B�&B�B��B��B��B��B��B��B��B��B�{B�pBeB�mB�tB�oB�hB�iB�gBfBaBbB~YB|PBy<Bw4BtBrBqBoBl�Bk�Bm�Bm�BoBqBu(BsBqBoBoBp	Bp	BtBv*By=B�gB��B��B�nB~^B}VBz?Bw5Bt!Bt Bx5Bx9Bx8Bw4Bw2Bx8Bx:Bx6Bx9Bw5Bv-Bv,BtBrBo Bl�Bl�BpBqBpBrBu&Bv(Bx8By<B{LB{KB{KB|OB}WBaB�oB��B��B��B�B�B�	B��B��B�B�sB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�&B�/B�.B�+B�'B�/B�1B�AB�`B�rBقBۋBیBܓB�B�B��B��B��B��B�B�B�B�CB	 jB	uB	�B	�B	�B	
�B	�B	�B	�B	�B	B	 B	B	"B	$@B	'SB	&IB	&NB	*fB	/�B	2�B	3�B	6�B	9�B	;�B	@�B	B�B	EB	HB	M7B	N>B	OBB	PJB	PHB	RWB	TcB	TaB	UeB	WuB	WuB	WvB	Y�B	]�B	b�B	c�B	d�B	f�B	g�B	h�B	i�B	i�B	k�B	m�B	u)B	w2B	x7B	y>B	y<B	zDB	|OB	}XB	}WB	~^B	�iB	�qB	�tB	��B	��B	��B	��B	��B	�|B	bB	�kB	�nB	��B	��B	��B	�B	�1B	�4B	�BB	�IB	�_B	�jB	�VB	�VB	�QB	�UB	�PB	�vB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�$B	�#B	�7B	�8B	�DB	�XB	�^B	�_B	�ZB	�VB	�sB	چB	ܗB	ߤB	�B	ߥB	ߨB	ߤB	ݗB	ܔB	�B	ݛB	ܔB	ݚB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�5B	�9B	�FB	�DB	�GB	�BB	�IB	�NB	�BB	�DB	�RB	�TB	�QB	�YB	�UB	�QB	�PB	�_B	�eB
 jB	�bB	�eB	�eB	�cB	�fB
qB
qB
qB
vB
tB
vB
{B
�B
�B
�B
�B
�B
�B
	�B
�B
�B
!2B
)_B
0�B
7�B
=�B
DB
M5B
TaB
[�B
b�B
g�B
j�B
p	B
u'B
x8B
{HB
bB
�uB
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =0(+/-0.002) in PSS-78.                                                                                                                                                                                                 Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940012019060409400120190604094001  AO  ARCAADJP                                                                    20160415191634    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160415191634  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160415191634  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094001  IP                  G�O�G�O�G�O�                