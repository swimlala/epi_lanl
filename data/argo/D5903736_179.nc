CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:57Z creation      
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041157  20190604094026  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @���L;C1   @�����ir@4|�hs�d�+I�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyw�D�  D�;3D���D�� D�3D�<)D�}D��RD�D�Q�D�uqD��D�)D�C3D�
D��D��D�;3D�}qD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\)@e@��H@��HAp�A9p�AYp�Ayp�A��RA��RA��RA��RA̸RAܸRA�RA��RB\)B\)B\)B\)B&\)B.\)B6\)B>\)BF\)BN\)BV\)B^\)Bf\)Bn\)Bv\)B~\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C�
C�
C�
C�
C	�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C!�
C#�
C%�
C'�
C)�
C+�
C-�
C/�
C1�
C3�
C5�
C7�
C9�
C;�
C=�
C?�
CA�
CC�
CE�
CG�
CI�
CK�
CM�
CO�
CQ�
CS�
CU�
CW�
CY�
C[�
C]�
C_�
Ca�
Cc�
Ce�
Cg�
Ci�
Ck�
Cm�
Co�
Cq�
Cs�
Cu�
Cw�
Cy�
C{�
C}�
C�
C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅C�˅D e�D ��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��D	e�D	��D
e�D
��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��De�D��D e�D ��D!e�D!��D"e�D"��D#e�D#��D$e�D$��D%e�D%��D&e�D&��D'e�D'��D(e�D(��D)e�D)��D*e�D*��D+e�D+��D,e�D,��D-e�D-��D.e�D.��D/e�D/��D0e�D0��D1e�D1��D2e�D2��D3e�D3��D4e�D4��D5e�D5��D6e�D6��D7e�D7��D8e�D8��D9e�D9��D:e�D:��D;e�D;��D<e�D<��D=e�D=��D>e�D>��D?e�D?��D@e�D@��DAe�DA��DBe�DB��DCe�DC��DDe�DD��DEe�DE��DFe�DF��DGe�DG��DHe�DH��DIe�DI��DJe�DJ��DKe�DK��DLe�DL��DMe�DM��DNe�DN��DOe�DO��DPe�DP��DQe�DQ��DRe�DR��DSe�DS��DTe�DT��DUe�DU��DVe�DV��DWe�DW��DXe�DX��DYe�DY��DZe�DZ��D[e�D[��D\e�D\��D]e�D]��D^e�D^��D_e�D_��D`e�D`��Dae�Da��Dbe�Db��Dce�Dc��Dde�Dd��Dee�De��Dfe�Df��Dge�Dg��Dhe�Dh��Die�Di��Dje�Dj��Dke�Dk��Dle�Dl��Dme�Dm��Dne�Dn��Doe�Do��Dpe�Dp��Dqe�Dq��Dre�Dr��Dse�Ds��DtX�Dy]qD��D�.D�x�D���D��D�/
D�p D��3D��D�D�D�hRD���D�
D�6D�q�D� D���D�.D�pRD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�ȴA�ȴA�ĜA̍PA�7LA�AˋDAʩ�A��#AȁA���A���A��/A���Aģ�A�dZA�&�AøRA�?}A��mA§�AA�9XA���A�v�A��yA�?}A��A��wA���A�ZA���A�ȴA�t�A�I�A���A��RA��A��
A�G�A���A��wA��;A��uA�/A��A�I�A�hsA��/A��A�bA��/A���A��TA��A��FA��`A�jA�JA�=qA�1'A�1'A�  A�"�A��/A���A�l�A��A�ZA�A�"�A�\)A�jA�?}A���A���A���A���A�
=A�7LA� �A���A��jA�r�A��A�{A��A���A���A��A��^A�O�A�VA�33A���A�M�A�%A���A�l�A�|�A�VA�&�A��A��A�jA�t�A�p�A��\A�  A�{A|�`A{&�Ax �AvffAv-At��Aq�Ap�uAo/Am�
Am�Ak��AjA�Ai��AfbNAc"�Aa%A`A�A_��A^I�A]�TA\��A[��AY��AU�wATn�ASXARjAQ�APjAP5?AO�AN��AMVAKAKAI�-AH�AHbAF�RAEt�AD��AC��AB��AA��AAXAA�A@1A>z�A>{A=�A<��A<=qA:9XA8�/A8jA7�A7�
A7|�A6�A5��A4ȴA3XA1��A1/A.��A-�#A-7LA,��A,v�A+�
A)�;A(Q�A'|�A&jA%ƨA%`BA$jA#G�A"Ax�A�+A�
A1A1A��A�`Ax�AA�+A��A��AĜA�hAƨA
=A^5A��A
E�A	33A-A\)AĜAƨA�uA�AĜA�A%A9XA$�A��@��F@�%@��@�ff@��u@��h@�R@�`B@�
=@�@�@�1@�7@�Z@�@�33@�O�@�@��@ߝ�@�K�@޸R@�7L@ڗ�@���@�Ĝ@؃@�Q�@��@��H@�7L@�\)@��@���@�l�@�;d@�
=@�V@Ͳ-@͉7@�V@�A�@��@ʸR@ʗ�@�n�@�$�@��@�hs@�9X@ƸR@���@���@�v�@�E�@�@���@�x�@�%@���@�A�@�o@�V@��7@�%@�Q�@��F@���@�x�@��@���@�ff@��T@�7L@�7L@�G�@���@��w@��@��@�{@��7@���@�z�@�b@�dZ@��@�ȴ@�V@�=q@�=q@�$�@��@���@�O�@���@���@���@��@��@�7L@��`@��@�"�@�n�@�x�@��j@��@���@���@�;d@���@���@�O�@���@�I�@�1@�ƨ@��P@�+@��R@���@��\@�n�@�^5@�M�@�@���@�hs@�?}@��-@�x�@�%@�X@��@�bN@�I�@�ƨ@��@��;@�1'@��!@�^5@�V@�{@�J@���@�G�@��@��/@��j@���@��/@���@�z�@�ƨ@��@���@�|�@�t�@�\)@�dZ@�K�@�33@��@��R@���@�n�@�$�@��@���@�X@�/@�%@���@���@��`@���@�Ĝ@��j@��j@��9@��9@��@���@���@��u@��D@�z�@�bN@�(�@��F@��P@�S�@�K�@�C�@��@���@��@���@�n�@��@�&�@�&�@�%@��@���@��j@���@��/@��/@���@���@��/@�r�@�(�@��@�S�@�C�@��H@�=q@�@�`B@��@��j@��@���@��u@��D@��D@�9X@�1@�|�@�l�@�K�@���@��@�ȴ@�v�@�ff@�ff@��@�@�x�@���@�r�@��m@���@�S�@��@��H@��R@���@��+@�v�@�E�@�@���@��/@��D@�z�@��@�Z@�K�@�o@���@�_@wC�@n��@e��@_+@Xz�@Q��@HI�@FH�@?�@;8@3�@-��@*_�@#�Q@7@V�@�@@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A�ȴA�ȴA�ĜA̍PA�7LA�AˋDAʩ�A��#AȁA���A���A��/A���Aģ�A�dZA�&�AøRA�?}A��mA§�AA�9XA���A�v�A��yA�?}A��A��wA���A�ZA���A�ȴA�t�A�I�A���A��RA��A��
A�G�A���A��wA��;A��uA�/A��A�I�A�hsA��/A��A�bA��/A���A��TA��A��FA��`A�jA�JA�=qA�1'A�1'A�  A�"�A��/A���A�l�A��A�ZA�A�"�A�\)A�jA�?}A���A���A���A���A�
=A�7LA� �A���A��jA�r�A��A�{A��A���A���A��A��^A�O�A�VA�33A���A�M�A�%A���A�l�A�|�A�VA�&�A��A��A�jA�t�A�p�A��\A�  A�{A|�`A{&�Ax �AvffAv-At��Aq�Ap�uAo/Am�
Am�Ak��AjA�Ai��AfbNAc"�Aa%A`A�A_��A^I�A]�TA\��A[��AY��AU�wATn�ASXARjAQ�APjAP5?AO�AN��AMVAKAKAI�-AH�AHbAF�RAEt�AD��AC��AB��AA��AAXAA�A@1A>z�A>{A=�A<��A<=qA:9XA8�/A8jA7�A7�
A7|�A6�A5��A4ȴA3XA1��A1/A.��A-�#A-7LA,��A,v�A+�
A)�;A(Q�A'|�A&jA%ƨA%`BA$jA#G�A"Ax�A�+A�
A1A1A��A�`Ax�AA�+A��A��AĜA�hAƨA
=A^5A��A
E�A	33A-A\)AĜAƨA�uA�AĜA�A%A9XA$�A��@��F@�%@��@�ff@��u@��h@�R@�`B@�
=@�@�@�1@�7@�Z@�@�33@�O�@�@��@ߝ�@�K�@޸R@�7L@ڗ�@���@�Ĝ@؃@�Q�@��@��H@�7L@�\)@��@���@�l�@�;d@�
=@�V@Ͳ-@͉7@�V@�A�@��@ʸR@ʗ�@�n�@�$�@��@�hs@�9X@ƸR@���@���@�v�@�E�@�@���@�x�@�%@���@�A�@�o@�V@��7@�%@�Q�@��F@���@�x�@��@���@�ff@��T@�7L@�7L@�G�@���@��w@��@��@�{@��7@���@�z�@�b@�dZ@��@�ȴ@�V@�=q@�=q@�$�@��@���@�O�@���@���@���@��@��@�7L@��`@��@�"�@�n�@�x�@��j@��@���@���@�;d@���@���@�O�@���@�I�@�1@�ƨ@��P@�+@��R@���@��\@�n�@�^5@�M�@�@���@�hs@�?}@��-@�x�@�%@�X@��@�bN@�I�@�ƨ@��@��;@�1'@��!@�^5@�V@�{@�J@���@�G�@��@��/@��j@���@��/@���@�z�@�ƨ@��@���@�|�@�t�@�\)@�dZ@�K�@�33@��@��R@���@�n�@�$�@��@���@�X@�/@�%@���@���@��`@���@�Ĝ@��j@��j@��9@��9@��@���@���@��u@��D@�z�@�bN@�(�@��F@��P@�S�@�K�@�C�@��@���@��@���@�n�@��@�&�@�&�@�%@��@���@��j@���@��/@��/@���@���@��/@�r�@�(�@��@�S�@�C�@��H@�=q@�@�`B@��@��j@��@���@��u@��D@��D@�9X@�1@�|�@�l�@�K�@���@��@�ȴ@�v�@�ff@�ff@��@�@�x�@���@�r�@��m@���@�S�@��@��H@��R@���@��+@�v�@�E�@�@���@��/@��D@�z�@��@�Z@�K�G�O�@���@�_@wC�@n��@e��@_+@Xz�@Q��@HI�@FH�@?�@;8@3�@-��@*_�@#�Q@7@V�@�@@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�3B�3B�3B�3B�LB��B�B�BBB/BJ�B`BBcTBdZBhsBn�Bn�Bp�Bp�Bs�Bx�Bz�B|�B~�B�B�7B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�9B�9B�'B�B��B�!B�B�B�!B�}BÖB�ZB��B��B�B�B�`B�HB�B��B��BĜB�^B�?B�-B�B��B��B�hB�7B}�Bk�BaHB[#BN�B<jB1'B(�B�B{BbBVB	7B��B�B�
B�wB��B��B��B�hB�7Bp�BI�BA�B=qB8RB1'B"�B\BB
��B
�B
�/B
��B
�FB
��B
��B
��B
�B
t�B
cTB
XB
T�B
J�B
9XB
1'B
'�B
 �B
�B
hB

=B
B	�B	��B	��B	�^B	�9B	�B	��B	��B	��B	�VB	s�B	jB	cTB	\)B	VB	Q�B	O�B	K�B	E�B	=qB	6FB	2-B	+B	$�B	!�B	�B	�B	bB	DB		7B	B	B	  B��B��B��B��B�B�B�B�sB�mB�`B�ZB�NB�5B�#B�B��B��B��BɺBƨBĜBÖB��B�wB�LB�3B�!B�B�B��B��B��B��B��B��B�{B�hB�PB�=B�+B�B�B�B~�B|�Bz�Bx�Bw�Bv�Bu�Bt�Bt�Bs�Bs�Bs�Br�Bx�B��B��B��B��B�{B�hB�hB�VB�7B�%B�B�B�%B�7B�DB�JB�PB�DB�=B�7B�VB�\B�VB�PB�VB�VB�\B�bB�\B�\B�VB�hB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B�B�?B�LB�dB�dBÖBǮBȴBȴBȴBɺB��B�B�#B�B�B�5B�BB�HB�ZB�ZB�`B�fB�fB�sB�B�B�B�B��B��B��B��B��B��B��B	B	B	B	%B		7B	
=B	JB	JB	VB	bB	uB	�B	�B	!�B	%�B	)�B	+B	+B	+B	)�B	)�B	+B	,B	-B	/B	49B	7LB	>wB	C�B	D�B	H�B	N�B	T�B	VB	VB	[#B	`BB	gmB	m�B	k�B	jB	iyB	jB	k�B	m�B	n�B	o�B	q�B	r�B	s�B	s�B	s�B	t�B	t�B	v�B	x�B	z�B	~�B	�B	� B	�B	�%B	�B	�%B	�%B	�+B	�JB	�bB	�JB	�PB	�\B	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�LB	�RB	�XB	�qB	ÖB	ĜB	ŢB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�TB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
%B
%B
%B
+B
	7B

=B
�B
�B
'�B
/�B
7LB
=�B
B�B
G�B
OvB
P�B
VB
Y�B
_�B
b�B
e�B
j�B
qvB
xRB
}�B
�B
�G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B�B˭B��B��B��B'�BC{BX�B\B]Ba)BgNBgQBiWBiWBlkBq�Bs�Bu�Bw�By�B��B�B�9B�kB��B��B��B��B��B�lB�fB�ZB�4B�QB��B��B�B��B��B��B��B��B��B��B��B��B��B�3B�LB�B�B�oB�SB�;B�B��B��B͵BɞB�OB�B��B��B��B��B�uB�&B��Bv�BdCBZ
BS�BG�B50B)�B!�B�BEB	&BB�B�B�JB��B�AB��B�vB�^B�8B�BivBB�B:aB6FB1&B)�B�B4B
��B
��B
�hB
�B
ǻB
�$B
��B
��B
�jB
z�B
m�B
\<B
P�B
M�B
C�B
2BB
*B
 �B
�B
�B

VB
(B	��B	�B	��B	�{B	�OB	�*B	��B	��B	��B	��B	�NB	l�B	cvB	\OB	U B	N�B	J�B	H�B	D�B	>�B	6lB	/AB	++B	$ B	�B	�B	�B	�B		bB	DB	6B�B�B��B��B��B��B��B�B�B�B�xB�qB�gB�`B�PB�<B�)B�B�B��B��B��B��B��B��B��B��B�QB�=B�+B�B�B�B��B��B��B��B��B��B�tB�\B�IB�:B~.B})BzBxBu�Bs�Bq�Bp�Bo�Bn�Bm�Bm�Bl�Bl�Bl�Bk�Bq�B��B��B��B��B��B�vB�zB�eB�IB4B~3B}.B6B�JB�SB�^B�eB�ZB�PB�LB�nB�mB�mB�bB�hB�iB�oB�sB�nB�qB�gB�~B��B��B��B��B��B��B��B��B��B��B�B�B�B�%B�RB�]B�yB�vB��B��B��B��B��B��B��B�B�4B�!B�1B�EB�RB�^B�lB�mB�rB�uB�zB�B�B�B�B��B��B��B��B��B��B��B� B�B�B�B�8B	HB	LB	\B	\B	hB		oB	�B	�B	�B	�B	�B	#B	$B	$B	$B	#B	#B	$B	%B	&B	(/B	-HB	0[B	7�B	<�B	=�B	A�B	G�B	NB	OB	OB	T0B	YQB	`vB	f�B	d�B	c�B	b�B	c�B	d�B	f�B	g�B	h�B	j�B	k�B	l�B	l�B	l�B	m�B	m�B	o�B	q�B	s�B	x
B	zB	yB	~)B	3B	~,B	/B	-B	�:B	�WB	�oB	�UB	�_B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�#B	�>B	�VB	�]B	�eB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�"B	�%B	�&B	�*B	�%B	�%B	�$B	�B	�1B	�<B	�@B	�IB	�PB	�NB	�QB	�UB	�WB	�YB	�^B	�ZB	�bB	�aB	�\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�%B	�/B	�.B	�.B
 .B
>G�O�B
�B
�B
 �B
(�B
0NB
6�B
;�B
@�B
H|B
I�B
O&B
R�B
X�B
[�B
^�B
c�B
j}B
qWB
v�B
x�B
|G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.41 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.007(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940262019060409402620190604094026  AO  ARCAADJP                                                                    20181121041157    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041157  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041157  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094026  IP                  G�O�G�O�G�O�                