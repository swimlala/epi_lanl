CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-01-22T01:01:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180122010116  20190604094143  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�Fk�-��1   @�Flr��V@4�^5?|��d}?|�h1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyQHD��D�J=D��D��D��D�F�D��D��\D��D�2�D�k�D��
D��{D�L�D�)�D�њD��D�K�D�w\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_�D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dtn�DyL�D��D�H D�}qD���D�	HD�D{D���D��D���D�0RD�iHD���D��>D�J�D�']D��]D���D�IHD�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A�1'A�1'A�33A�5?A�5?A�5?A�5?A�9XA�;dA�9XA�7LA�9XA�E�A�K�A�I�A�G�A�G�A�E�A�E�A�=qA�(�A��A���A���Aǝ�Aƴ9AŋDA��AēuA�ffA�7LA��A�A���A��`A�ĜAöFAô9Aú^Aú^Aá�AÍPAÅA�z�A�jA�ffA�^5A�K�A�1A�A��#A��A�33A���A�I�A��mA�&�A�$�A�9XA�O�A�x�A�G�A�  A�A��uA��A���A�A�E�A�I�A��7A�-A�VA�x�A���A��A�ȴA�~�A�%A��RA�E�A��TA��A�p�A��TA�5?A���A�M�A�Q�A��A�`BA���A��A��A�r�A���A�^5A�5?A���A���A�A�A�?}A�Q�A�+A���A�hsA�&�A��jA��A���A��wA�/A�p�A���A~��A~A�A}?}A{�PAy�Aw��Av��Au7LAr~�Ap �An�`Al�+Aj�AiXAh1'Af��Ac��Aa&�A`(�A_�-A_�A]XA[;dAY�;AY\)AY%AX�AX$�AW�AV��AVE�AUdZAS��AR��AQ��AP1'AO��AN��AM��AMS�AL�!AK��AJ��AIC�AH�AH  AGAG�AFn�AES�AD1'ABȴAA�A?��A>�A=7LA;dZA:��A9XA8n�A7/A6�9A6�\A5�;A4��A2ĜA1�;A0$�A.�A-�PA,��A*��A)�FA(��A'��A&ĜA%��A$�A#��A"��A �/A JA|�A��AG�AQ�A�A�A|�A�`A�;AoA�A��Ap�AA��A�yA5?A��AAn�A+A
��A�RA�AƨA~�At�A+A
=AA��A�!AA�A/A1A��@���@���@�1'@��
@�|�@�X@��\@�j@��@�n�@�O�@�S�@�"�@�ff@��@��y@��/@���@��@���@�G�@ߝ�@܃@�^5@�(�@�33@��@�b@�;d@���@�p�@���@�$�@��@�x�@���@�ƨ@�v�@��@�
=@���@��`@�I�@���@�@�-@�?}@���@�z�@�A�@���@���@���@�V@���@�/@���@��@�(�@�K�@���@�{@��@�Ĝ@�r�@�(�@���@�ƨ@�t�@��\@��@���@�/@�bN@��m@��w@�C�@��R@�n�@���@���@���@� �@�+@��@��+@�{@�x�@�G�@���@��u@�(�@���@�;d@�o@���@��+@�M�@���@��h@��h@��7@��@�`B@�&�@��/@��u@��@��m@��@�\)@��+@�@���@��@��@�j@�I�@��;@�o@��@��H@�ȴ@�v�@�E�@�J@��-@��@�%@���@�1@���@�dZ@�33@�"�@��y@�ȴ@��R@���@�n�@�ff@�J@��h@�/@���@��@�(�@��;@��
@�ƨ@��@�\)@�o@��H@�^5@�@�G�@���@��@���@���@�r�@�9X@�1@���@�\)@��@���@��w@��y@��@�@���@��@�/@��j@��D@�I�@�  @��@�ƨ@���@�K�@�
=@��y@��@���@��\@�$�@��@�@���@��7@�G�@���@��u@�bN@��@��@��w@���@�l�@�K�@�C�@�o@��@��y@��R@�M�@�-@���@��-@��-@���@��h@�`B@��@��@��j@��@��u@���@�\)@�33@�"�@�o@���@��H@�ȴ@���@���@��\@�~�@�ff@�V@�-@�@�p�@�X@�?}@��/@�Ĝ@��j@��9@���@���@��u@��D@�z�@�Q�@�I�@�Q�@�r�@��D@�I�@��@~��@t��@lZ@cj�@[�@T�9@L�D@D>B@=\�@7�;@2�F@-��@(�@#�
@{�@i�@kQ@ff@J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111A�(�A�1'A�1'A�33A�5?A�5?A�5?A�5?A�9XA�;dA�9XA�7LA�9XA�E�A�K�A�I�A�G�A�G�A�E�A�E�A�=qA�(�A��A���A���Aǝ�Aƴ9AŋDA��AēuA�ffA�7LA��A�A���A��`A�ĜAöFAô9Aú^Aú^Aá�AÍPAÅA�z�A�jA�ffA�^5A�K�A�1A�A��#A��A�33A���A�I�A��mA�&�A�$�A�9XA�O�A�x�A�G�A�  A�A��uA��A���A�A�E�A�I�A��7A�-A�VA�x�A���A��A�ȴA�~�A�%A��RA�E�A��TA��A�p�A��TA�5?A���A�M�A�Q�A��A�`BA���A��A��A�r�A���A�^5A�5?A���A���A�A�A�?}A�Q�A�+A���A�hsA�&�A��jA��A���A��wA�/A�p�A���A~��A~A�A}?}A{�PAy�Aw��Av��Au7LAr~�Ap �An�`Al�+Aj�AiXAh1'Af��Ac��Aa&�A`(�A_�-A_�A]XA[;dAY�;AY\)AY%AX�AX$�AW�AV��AVE�AUdZAS��AR��AQ��AP1'AO��AN��AM��AMS�AL�!AK��AJ��AIC�AH�AH  AGAG�AFn�AES�AD1'ABȴAA�A?��A>�A=7LA;dZA:��A9XA8n�A7/A6�9A6�\A5�;A4��A2ĜA1�;A0$�A.�A-�PA,��A*��A)�FA(��A'��A&ĜA%��A$�A#��A"��A �/A JA|�A��AG�AQ�A�A�A|�A�`A�;AoA�A��Ap�AA��A�yA5?A��AAn�A+A
��A�RA�AƨA~�At�A+A
=AA��A�!AA�A/A1A��@���@���@�1'@��
@�|�@�X@��\@�j@��@�n�@�O�@�S�@�"�@�ff@��@��y@��/@���@��@���@�G�@ߝ�@܃@�^5@�(�@�33@��@�b@�;d@���@�p�@���@�$�@��@�x�@���@�ƨ@�v�@��@�
=@���@��`@�I�@���@�@�-@�?}@���@�z�@�A�@���@���@���@�V@���@�/@���@��@�(�@�K�@���@�{@��@�Ĝ@�r�@�(�@���@�ƨ@�t�@��\@��@���@�/@�bN@��m@��w@�C�@��R@�n�@���@���@���@� �@�+@��@��+@�{@�x�@�G�@���@��u@�(�@���@�;d@�o@���@��+@�M�@���@��h@��h@��7@��@�`B@�&�@��/@��u@��@��m@��@�\)@��+@�@���@��@��@�j@�I�@��;@�o@��@��H@�ȴ@�v�@�E�@�J@��-@��@�%@���@�1@���@�dZ@�33@�"�@��y@�ȴ@��R@���@�n�@�ff@�J@��h@�/@���@��@�(�@��;@��
@�ƨ@��@�\)@�o@��H@�^5@�@�G�@���@��@���@���@�r�@�9X@�1@���@�\)@��@���@��w@��y@��@�@���@��@�/@��j@��D@�I�@�  @��@�ƨ@���@�K�@�
=@��y@��@���@��\@�$�@��@�@���@��7@�G�@���@��u@�bN@��@��@��w@���@�l�@�K�@�C�@�o@��@��y@��R@�M�@�-@���@��-@��-@���@��h@�`B@��@��@��j@��@��u@���@�\)@�33@�"�@�o@���@��H@�ȴ@���@���@��\@�~�@�ff@�V@�-@�@�p�@�X@�?}@��/@�Ĝ@��j@��9@���@���@��u@��D@�z�@�Q�@�I�@�Q�@�r�@��DG�O�@��@~��@t��@lZ@cj�@[�@T�9@L�D@D>B@=\�@7�;@2�F@-��@(�@#�
@{�@i�@kQ@ff@J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�FB�FB�FB�FB�FB�LB�^B�jB��BƨB��B�B&�B@�BF�BI�BM�BO�BP�BQ�BR�BVBXBZB_;BffBgmBgmBgmBgmBffBffBgmBffBgmBm�Bv�B� B�B�7B�DB�7B�%B�+B~�Bx�By�Bz�Bv�Bq�Bu�B|�B}�Bz�Bs�Bm�BjB_;BZBQ�BH�B:^B+B+B.B)�B�BoBPBB��B�B��B�jB�B�?B�'B��B�Bv�Bp�Bp�By�Bw�Bo�BK�B(�B%B
�B
�B
�B
�BB
��B
ɺB
��B
�RB
��B
��B
�bB
|�B
o�B
iyB
bNB
W
B
H�B
@�B
9XB
/B
 �B
{B
JB	��B	�B	�B	�TB	�B	ɺB	�wB	�XB	�FB	�'B	��B	��B	��B	��B	�{B	�hB	�\B	�JB	�1B	�B	� B	w�B	s�B	m�B	gmB	cTB	_;B	[#B	XB	T�B	O�B	J�B	D�B	?}B	>wB	<jB	:^B	5?B	/B	(�B	!�B	�B	{B	PB	B��B��B��B�B�B�yB�mB�ZB�5B�
B��B��BÖB�dB�?B�B��B��B��B��B��B��B��B�hB�VB�DB�1B�%B�B�B�B� B}�B{�Bz�Bw�Bu�Bu�Bs�Bq�Bo�Bl�Bk�BiyBhsBffBbNB`BB^5B]/B[#B[#B[#B[#B[#B[#B[#BZB[#B\)BZBXBYB[#B^5B_;B^5BaHBbNBaHBcTBbNBaHBcTBdZBe`BgmBgmBjBl�Bm�Bm�Bm�Bm�Bo�Bq�Bv�Bv�Bx�B{�B|�B�B�B�B�=B�=B�DB�DB�VB�oB��B��B��B��B��B��B��B��B�B�B�B�B�B�?B�FB�LB�RB�dB�qB�}B��BĜBǮBɺB��B��B��B��B�
B�
B�B�B�)B�/B�BB�fB�B�B�B�B�B��B��B	B	B	1B	
=B	JB	bB	{B	�B	�B	�B	�B	!�B	$�B	%�B	(�B	,B	.B	6FB	8RB	8RB	8RB	8RB	9XB	:^B	<jB	=qB	?}B	B�B	E�B	G�B	L�B	P�B	P�B	Q�B	S�B	[#B	]/B	^5B	^5B	`BB	aHB	ffB	hsB	k�B	n�B	r�B	s�B	u�B	x�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�PB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�?B	�RB	�XB	�RB	�RB	�LB	�LB	�LB	�RB	�dB	�jB	�qB	�}B	�}B	��B	ÖB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�/B	�)B	�/B	�;B	�BB	�HB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
1B
PB
�B
,WB
3�B
<PB
A�B
DB
K�B
N�B
T�B
W�B
]B
a�B
e�B
j�B
o�B
r�B
utB
z^B
.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�%B�IB�GB�JB�HB�JB�SB�bB�oB��B��B��B�B�B2B8�B;�B?�BA�BB�BC�BD�BG�BI�BLBQ0BXWBY^BY^BYcBY^BX[BX`BY]BX[BY`B_�Bh�Bq�BwB{*B}5B{)BxByBp�Bj�Bk�Bl�Bh�Bc�Bg�Bn�Bo�Bl�Be�B_�B\yBQ6BLBC�B:�B,[BB�B B�B
�BoB�SB�$B��BߝB�B�|B�.B�RB�7B��Bu&Bh�Bb�Bb�Bk�Bi�Ba�B=�BB
�GB
ްB
��B
��B
�hB
�	B
��B
��B
��B
�$B
��B
��B
o)B
a�B
[�B
T�B
IKB
:�B
2�B
+�B
!`B
B
�B	��B	�:B	�B	��B	բB	�lB	�B	��B	��B	��B	�|B	�IB	�B	��B	��B	��B	��B	��B	~�B	z�B	wzB	rYB	j'B	fB	_�B	Y�B	U�B	Q�B	M�B	JoB	G[B	B<B	=B	6�B	1�B	0�B	.�B	,�B	'�B	!~B	\B	0B	�B	�B��B��B�`B�IB�&B�B��B��B��B��BУB�}B�_B�4B�
B��B��B�xB�ZB�LB�BB�0B�B�B��B��B��B}�Bz�Bx�Bw�Bu�Bt�BrBpuBncBm_BjPBhDBhGBf5Bd'Bb!B_B^	B[�BZ�BX�BT�BR�BP�BO�BM�BM�BM�BM�BM�BM�BM�BL�BM�BN�BL�BJ�BK�BM�BP�BQ�BP�BS�BT�BS�BU�BT�BS�BU�BV�BW�BY�BY�B]B_B`B`B`B`Bb&Bd5BiRBiQBk\BnqBosBs�Bt�Bu�B|�B|�B}�B}�B��B��B�B� B�-B�EB�PB�PB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�<B�ZB�gB�mB�~BɍBɌBʑB˕BΩBϱBҿB��B��B�B�B�*B�-B�NB�oB�B��B��B��B��B	�B	�B	�B	B	&B	4B	IB	[B	`B	oB	�B	 �B	(�B	*�B	*�B	*�B	*�B	+�B	,�B	.�B	/�B	1�B	5B	8B	:'B	?FB	C^B	C[B	DfB	FpB	M�B	O�B	P�B	P�B	R�B	S�B	X�B	Z�B	]�B	aB	e$B	f-B	h8B	kHB	n[B	qkB	ruB	syB	s}B	u�B	u�B	u�B	u�B	w�B	w�B	z�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�1B	�CB	�RB	�ZB	�]B	�cB	�dB	�kB	�oB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�2B	�7B	�@B	�HB	�QB	�^B	�wB	�|B	̆B	͏B	ϚB	ΓB	ϛB	ѥB	ҭB	ӲB	վB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	� B	�B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�7B	�6B	�:B	�>B	�EB	�DB	�BB	�EB	�AB	�GB	�QB	�QB	�MB	�WB	�WB	�ZB	�XB	�\B	�fB	�qB	�yB	��B	��G�O�B	��B
,B
�B
&7B
.�B
4ZB
6|B
=�B
AB
F�B
J[B
OwB
T1B
XHB
]/B
bB
eHB
g�B
l�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941442019060409414420190604094144  AO  ARCAADJP                                                                    20180122010116    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180122010116  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180122010116  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094144  IP                  G�O�G�O�G�O�                