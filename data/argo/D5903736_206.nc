CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-01-01T08:01:27Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20180101080127  20190604094031  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�AM�Z�h1   @�ANU�s�@5�-�d�&�x��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyO\D��D�T�D���D���D�3D�<)D�o
D��{D�\D�MqD�q�D�ƸD��
D�=Dژ�D��HD�
=D�A�D�nD�{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��DthRDyJ�D��D�R�D���D�ƹD� �D�9�D�l�D��>D�D�K4D�o�D��{D���D�:�DږgD��D� D�?]D�k�D�yH11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�S�A�VA�S�A�K�A�M�A�K�A�K�A�C�A��A�oAƋDA�z�A�^5A�33A�bA��A��`A���A���AŴ9Aũ�Aš�Aś�Aŕ�AŋDAŇ+AŅAŁA�|�A�z�A�z�A�z�A�z�A�v�A�v�A�v�A�v�A�v�A�t�A�r�A�n�A�ffA�bNA�bNA�`BA�$�AĶFA�dZA�E�A�p�A��PA�\)A�ZA���A�VA�XA�`BA�|�A�(�A���A�ĜA���A�l�A�/A��A���A�A��A�ZA���A�"�A�$�A���A��
A�E�A���A���A�ZA��A�+A��hA�ffA�oA�9XA�ffA��A���A��PA�A���A��A��
A��hA���A�G�A�ffA���A���A��\A�"�A�{A�JA�A�-A�x�A�VA���A� �A�  A��A~�A|�Az�jAy7LAx�Aw�FAw�-Aw�-Aw33At��As��Aq|�Ao�Am�PAl�yAl  Ai��Af�Afv�Ae�Ac�Aa\)A`  A_VA]�mAZ�RAX�AW�PAU��AR��AQ%AO\)AM�AK��AI��AIoAH�+AG�hAE�AEdZADȴAC\)AA��A?�;A>n�A<�A:�`A:(�A9�7A8VA6��A69XA4��A49XA3�FA333A2��A1/A0Q�A/�;A/p�A/`BA/33A.�RA-�FA,��A,bNA+7LA)�A(ĜA'A%�7A#�^A#��A#x�A#&�A"ZA!p�A�
A��A  Ap�A�HA5?AjA�A��AA\)A��A~�AjA�A�/A
ZA��A�/AA�AE�A�hA��A�9A  Ap�A ��A �yA �yA �yA ȴA 5?@�@��D@���@��h@�dZ@�v�@��@�I�@�;d@�G�@�~�@��m@�E�@�7L@��@�ff@�h@��
@ާ�@��@�O�@�z�@۾w@�{@��H@�{@�x�@ӕ�@�p�@�t�@�p�@��
@���@�5?@��@�p�@��@��
@�V@�%@öF@�1'@�(�@�1@�ƨ@þw@��;@�5?@��@�Ĝ@�I�@��@���@�|�@��@��+@���@�p�@�Ĝ@�(�@���@�S�@�ff@�@�%@�Q�@� �@���@��;@��@�t�@�dZ@�
=@��@�-@��h@��@��D@��@�5?@��@�  @�@���@���@��9@�Q�@�b@���@�+@��H@��+@�$�@�J@�@���@��T@��@�V@���@���@�1@��@��m@��@�v�@�J@��h@�?}@���@��D@�1'@���@���@��@�|�@�t�@�l�@�l�@�\)@�\)@��@�
=@�ȴ@�X@��@�r�@�Q�@�I�@�A�@�9X@�(�@��@��@�S�@�o@��y@��H@��H@��@���@���@���@��+@�M�@�J@��T@��h@�?}@�/@�/@���@���@��w@��@���@��H@�~�@�^5@�=q@��-@�&�@��`@��D@�r�@��D@��9@���@��/@��/@��/@�Ĝ@�Ĝ@��j@���@��u@�r�@�Q�@� �@���@�ƨ@��F@���@�\)@�o@���@��+@�E�@�5?@��@�@��T@���@��@�O�@���@�(�@���@���@�l�@�K�@��y@�n�@��#@���@�7L@���@��j@��@��u@�(�@��;@�|�@�33@��@��@���@��\@�V@�J@��-@��h@�G�@�/@�&�@��@�V@��/@��`@���@�z�@�Q�@�1'@�1@��m@���@�\)@�"�@�o@��H@��@���@��@�x�@�G�@�&�@���@��D@�z�@�Q�@���@�ƨ@��F@��@�|�@��P@�t�@�;d@�
=@��y@��H@��@���@�ff@�=q@�E�@�V@�M�@��"@��@t��@lU2@c�@@[��@T�@Mm]@G�@?j�@7خ@1��@-5�@'O@#@��@��@z�@�@;@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�S�A�VA�S�A�K�A�M�A�K�A�K�A�C�A��A�oAƋDA�z�A�^5A�33A�bA��A��`A���A���AŴ9Aũ�Aš�Aś�Aŕ�AŋDAŇ+AŅAŁA�|�A�z�A�z�A�z�A�z�A�v�A�v�A�v�A�v�A�v�A�t�A�r�A�n�A�ffA�bNA�bNA�`BA�$�AĶFA�dZA�E�A�p�A��PA�\)A�ZA���A�VA�XA�`BA�|�A�(�A���A�ĜA���A�l�A�/A��A���A�A��A�ZA���A�"�A�$�A���A��
A�E�A���A���A�ZA��A�+A��hA�ffA�oA�9XA�ffA��A���A��PA�A���A��A��
A��hA���A�G�A�ffA���A���A��\A�"�A�{A�JA�A�-A�x�A�VA���A� �A�  A��A~�A|�Az�jAy7LAx�Aw�FAw�-Aw�-Aw33At��As��Aq|�Ao�Am�PAl�yAl  Ai��Af�Afv�Ae�Ac�Aa\)A`  A_VA]�mAZ�RAX�AW�PAU��AR��AQ%AO\)AM�AK��AI��AIoAH�+AG�hAE�AEdZADȴAC\)AA��A?�;A>n�A<�A:�`A:(�A9�7A8VA6��A69XA4��A49XA3�FA333A2��A1/A0Q�A/�;A/p�A/`BA/33A.�RA-�FA,��A,bNA+7LA)�A(ĜA'A%�7A#�^A#��A#x�A#&�A"ZA!p�A�
A��A  Ap�A�HA5?AjA�A��AA\)A��A~�AjA�A�/A
ZA��A�/AA�AE�A�hA��A�9A  Ap�A ��A �yA �yA �yA ȴA 5?@�@��D@���@��h@�dZ@�v�@��@�I�@�;d@�G�@�~�@��m@�E�@�7L@��@�ff@�h@��
@ާ�@��@�O�@�z�@۾w@�{@��H@�{@�x�@ӕ�@�p�@�t�@�p�@��
@���@�5?@��@�p�@��@��
@�V@�%@öF@�1'@�(�@�1@�ƨ@þw@��;@�5?@��@�Ĝ@�I�@��@���@�|�@��@��+@���@�p�@�Ĝ@�(�@���@�S�@�ff@�@�%@�Q�@� �@���@��;@��@�t�@�dZ@�
=@��@�-@��h@��@��D@��@�5?@��@�  @�@���@���@��9@�Q�@�b@���@�+@��H@��+@�$�@�J@�@���@��T@��@�V@���@���@�1@��@��m@��@�v�@�J@��h@�?}@���@��D@�1'@���@���@��@�|�@�t�@�l�@�l�@�\)@�\)@��@�
=@�ȴ@�X@��@�r�@�Q�@�I�@�A�@�9X@�(�@��@��@�S�@�o@��y@��H@��H@��@���@���@���@��+@�M�@�J@��T@��h@�?}@�/@�/@���@���@��w@��@���@��H@�~�@�^5@�=q@��-@�&�@��`@��D@�r�@��D@��9@���@��/@��/@��/@�Ĝ@�Ĝ@��j@���@��u@�r�@�Q�@� �@���@�ƨ@��F@���@�\)@�o@���@��+@�E�@�5?@��@�@��T@���@��@�O�@���@�(�@���@���@�l�@�K�@��y@�n�@��#@���@�7L@���@��j@��@��u@�(�@��;@�|�@�33@��@��@���@��\@�V@�J@��-@��h@�G�@�/@�&�@��@�V@��/@��`@���@�z�@�Q�@�1'@�1@��m@���@�\)@�"�@�o@��H@��@���@��@�x�@�G�@�&�@���@��D@�z�@�Q�@���@�ƨ@��F@��@�|�@��P@�t�@�;d@�
=@��y@��H@��@���@�ff@�=q@�E�@�VG�O�@��"@��@t��@lU2@c�@@[��@T�@Mm]@G�@?j�@7خ@1��@-5�@'O@#@��@��@z�@�@;@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B�B.B.B-B.B0!B2-B49B7LB8RB:^B:^B;dB<jB>wB@�BA�BB�BC�BD�BD�BD�BD�BD�BE�BD�BD�BE�BE�BE�BE�BF�BF�BG�BG�BH�BK�BK�BM�BQ�BYBffB�+B�\B�VB�bB�oB�hB�hB�\B�\B�\B�VB�PB�DB�1B�Bz�Bn�Be`BT�BM�BH�BF�B9XB'�B!�B�B�BbB	7B	7BJBVB
=BBB��B��B�B�B�BBÖB�B�{B~�Br�BdZBH�B�B
��B
�ZB
��B
�B
��B
��B
��B
��B
��B
� B
u�B
k�B
`BB
S�B
J�B
C�B
A�B
@�B
?}B
:^B
-B
#�B
�B
JB
B	��B	��B	�yB	�)B	�
B	��B	ĜB	�dB	�9B	�B	��B	��B	�bB	�=B	~�B	s�B	iyB	`BB	XB	O�B	G�B	C�B	@�B	;dB	5?B	2-B	.B	'�B	�B	�B	bB	+B��B��B��B��B�B�B�`B�NB�BB�/B�B��B��B��B��B��B��BɺBŢB��B�}B�^B�9B�!B�B��B��B��B��B��B��B�oB�DB�B� B}�B{�Bx�Bt�Bq�Bn�Bk�Bk�BjBjBiyBgmBcTBcTBcTB`BB\)B[#B[#B^5B]/B[#B[#B`BBe`BiyBk�Bp�Bu�Bv�Bs�BjB]/BS�BO�BM�BL�BI�BK�BI�BK�BN�BO�BP�BS�BS�BS�BS�BW
BW
BVBT�BW
BVBT�BR�BO�BN�BQ�BYB`BBffBq�By�B~�B�B�PB�bB�hB�uB��B��B��B��B��B�B��B�B��B��B��B��B��B��B�B�B�B�B�!B�!B�'B�FB�LB�LB�XB�XB�^B�^B�dB�jB�wB��BĜBƨBǮBȴBɺB��B�B�#B�NB�fB�B�B�B�B�B��B��B��B	B	%B	+B	+B	+B	1B	DB	VB	PB	PB	�B	�B	 �B	&�B	/B	1'B	8RB	>wB	?}B	C�B	I�B	P�B	VB	XB	YB	YB	YB	ZB	ZB	ZB	]/B	^5B	`BB	^5B	_;B	e`B	ffB	ffB	ffB	ffB	ffB	ffB	gmB	gmB	hsB	jB	k�B	k�B	m�B	p�B	t�B	t�B	v�B	y�B	{�B	}�B	� B	�%B	�+B	�+B	�=B	�7B	�7B	�7B	�=B	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�?B	�LB	�LB	�XB	�XB	�^B	�jB	�wB	�}B	��B	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ŢB	ĜB	ŢB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�HB	�HB	�ZB	�ZB	�`B	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
+B
+B
+B
+B
1B
	7B
	7B
B
;B
,�B
4nB
:*B
@�B
F�B
K�B
O\B
WsB
\�B
bhB
f�B
m�B
pB
tnB
u�B
y�B
|�B
��B
�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�BB�@B�AB�@B�AB�BB�?B�DB�_B�B �B �B{B �B"�B$�B&�B)�B*�B,�B,�B-�B.�B0�B2�B3�B4�B6B7B7B7B7	B7B8B7B7B8B8
B8B8B9B9B:B:B; B>1B>-B@>BD\BK|BX�By�B��B��B��B��B��B��B��B��B��B��B�B}�Bz�Bv�BmLBaBW�BGpB@DB;'B9B+�BgB@BB
B�B��B��B��B �B��B��B�B�wB�WB�-B�B��B�B��B�Bq�Be@BV�B;IB	B
�{B
��B
�pB
��B
�9B
�1B
�B
��B
�5B
r�B
hpB
^3B
R�B
F�B
=tB
6FB
4<B
3:B
22B
-B
�B
�B
MB	�B	��B	�B	�B	�5B	��B	��B	��B	�ZB	�)B	��B	��B	��B	�YB	�*B	}B	q�B	f�B	\HB	SB	J�B	B�B	:�B	6hB	3XB	.=B	(B	%B	 �B	�B	�B	
hB	?B�B��B��B�B�B�xB�aB�AB�-B�!B�B��B��B��B��B��B��B��B��B��B�pB�aB�EB� B�B��B��B��B��B��B��B�sB�ZB~1Bt�Br�Bp�Bn�Bk�Bg�Bd�Ba�B^xB^xB]qB]qB\jBZbBVGBVIBVJBS3BO!BNBNBQ*BP'BNBNBS:BXSB\oB^{Bc�Bh�Bi�Bf�B]uBP%BF�BB�B@�B?�B<�B>�B<�B>�BA�BB�BC�BF�BF�BF�BF�BJBJBH�BG�BJBH�BG�BE�BB�BA�BD�BLBS=BYcBd�Bl�Bq�BwB�GB�[B�aB�mB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�:B�@B�?B�NB�LB�QB�SB�XB�^B�jB�}B��B��B��B��B��B��B��B�B�BB�TB�|B�B�B�B�B��B��B��B��B�B�B�B�B�B�5B	@B	 =B	 >B	mB	�B	�B	�B	"B	$B	+<B	1`B	2eB	6~B	<�B	C�B	H�B	J�B	K�B	K�B	K�B	MB	MB	MB	PB	QB	S+B	QB	R!B	XGB	YJB	YNB	YKB	YMB	YOB	YHB	ZQB	ZRB	[\B	]dB	^kB	^kB	`wB	c�B	g�B	g�B	i�B	l�B	n�B	p�B	r�B	y	B	zB	zB	} B	|B	|B	|B	}B	�9B	�EB	�XB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�
B	�B	�!B	�(B	�-B	�8B	�7B	�;B	�GB	�SB	�ZB	�fB	�sB	�tB	�{B	�|B	�~B	��B	��B	��B	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	ºB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�"B	�5B	�3B	�;B	�=B	�BB	�BB	�NB	�hB	�}B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�	B	�G�O�B
�B
B
bB
'BB
,�B
3�B
9aB
>�B
B1B
JIB
O�B
U:B
YVB
`�B
b�B
gAB
h�B
l{B
otB
ucB
vL11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940312019060409403120190604094031  AO  ARCAADJP                                                                    20180101080127    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180101080127  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180101080127  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094031  IP                  G�O�G�O�G�O�                