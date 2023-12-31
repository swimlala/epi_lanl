CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:10Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170910  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               JA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @غ���=�1   @غ��8�@6�������c��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    JA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�_\D���D���D�%D�c�D��)D��{D�RD�G�D���D��=D��D�I�Dڊ�D���D��D�VD��D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@��
@��
A�A=�A]�A}�A���A���A���A�(�A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��)C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0�HD1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��DnqHDn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt�{Dy��D���D�[3D��qD�ϮD� �D�_\D�� D��RD�)D�C�D���D��D��D�EqDچfD��D�qD�Q�D��D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�JA�
=A�
=A�VA�VA�JA�VA�oA��A�JA��;A�A��A��A��#A��TA�bNA��A��A��\A���A�n�A�;dA�ĜA���A���A�oA���A�-A��`A���A��hA�|�A�`BA�oA��
A��!A�r�A�5?A���A��;A��^A���A��hA�&�A���A��PA�\)A�9XA�33A�&�A��A��9A�`BA�bA��;A�^5A��9A��\A�XA�+A�{A�A�n�A�JA��mA�E�A�-A�=qA�;dA�A��;A���A�7LA�r�A�"�A�/A��-A�(�A��A���A�|�A�ȴA�ffA��A�&�A��!A�\)A��FA�t�A�-A��A�^5A��uA�&�A��A���A�ffA�Q�A�JA���A�JA���A��hA��A���A�~�A���A�/A���A��-A��yA���A��\A�1A�A�A��A�C�A�ȴA�$�A���A�;A|��Az �Ax�Aw&�Au+As��Aqt�AmAh5?Af�9Ad�Ac�A`��A^�/A]�PA\��A[��AY�mAWdZAU;dASS�AR��AR  APjAL�AK+AJA�AHbAE33ACABJA@�A>~�A>��A>(�A=��A=�A<bA;S�A;
=A:ZA8��A8Q�A7�A6bNA5ƨA4Q�A2��A1��A0�A0VA0E�A01A/S�A.�uA-�hA*�A)K�A(��A'�7A&�+A&bA%�A"�RA!�7A VA��A�/A-AO�A�A  AO�A��A�AƨAbA�A=qA��A�uAl�A
ZA	|�A��AĜAffA�A��AƨA�7A"�A�jA�A=qA`BA�HAM�A-A�A�TA�PA33A �A ~�A 9X@�\)@��@�@��@�r�@�"�@�@�r�@�"�@�-@��@��@�`B@���@�r�@�F@�\@���@��;@ꟾ@�Q�@�-@�&�@�P@��@�V@��@��@��@�h@�x�@�"�@�
=@�ȴ@�V@׶F@ԃ@҇+@ёh@�1'@���@ͩ�@̼j@̃@�I�@� �@� �@��@�  @˶F@˾w@�(�@�r�@�%@��@̴9@���@���@�^5@���@�`B@���@�1@��y@�n�@�/@�I�@���@�"�@��y@�=q@���@���@���@�
=@��@�@�@�5?@��7@���@��@��F@��P@�K�@��@��!@��!@��\@�~�@�ff@�V@�M�@�M�@�-@�x�@�%@�Ĝ@�z�@��m@��@�E�@���@��9@��@�K�@���@��@�@��H@���@��u@��@�z�@�r�@�(�@��w@�o@���@��R@���@��#@���@���@�G�@���@� �@�n�@���@�v�@���@��@���@�|�@���@�5?@�@�G�@��@��y@�^5@�5?@���@�=q@�M�@�5?@��T@���@��-@��h@�`B@�&�@���@��@�j@�(�@��w@�dZ@�ȴ@��R@��!@��+@�V@��@�@�p�@�&�@��@�(�@���@�\)@�33@�ȴ@�v�@�v�@���@�ȴ@��R@���@�ȴ@��R@���@��\@�-@��h@��@�X@��@��@���@�I�@�1@��
@��@�
=@��H@�ȴ@�ȴ@�V@�{@��@���@��7@�x�@�G�@��@��@���@���@�Z@���@���@���@���@�@��-@�x�@�hs@�hs@�?}@���@���@�Q�@��@��@�+@���@���@�^5@�M�@�-@�@�$�@�M�@�E�@���@���@���@���@�hs@�/@�(�@�  @��
@�|�@�t�@�dZ@�;d@�;d@�;d@�+@�o@��H@�~�@�ff@�V@�M�@�E�@�M�@�M�@�A @}�^@q�=@gH�@]�@V��@Nff@E�@@��@9��@4��@.�s@(�[@&_@ ֡@��@!�@�K@�U@1'@�.111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�%A�JA�
=A�
=A�VA�VA�JA�VA�oA��A�JA��;A�A��A��A��#A��TA�bNA��A��A��\A���A�n�A�;dA�ĜA���A���A�oA���A�-A��`A���A��hA�|�A�`BA�oA��
A��!A�r�A�5?A���A��;A��^A���A��hA�&�A���A��PA�\)A�9XA�33A�&�A��A��9A�`BA�bA��;A�^5A��9A��\A�XA�+A�{A�A�n�A�JA��mA�E�A�-A�=qA�;dA�A��;A���A�7LA�r�A�"�A�/A��-A�(�A��A���A�|�A�ȴA�ffA��A�&�A��!A�\)A��FA�t�A�-A��A�^5A��uA�&�A��A���A�ffA�Q�A�JA���A�JA���A��hA��A���A�~�A���A�/A���A��-A��yA���A��\A�1A�A�A��A�C�A�ȴA�$�A���A�;A|��Az �Ax�Aw&�Au+As��Aqt�AmAh5?Af�9Ad�Ac�A`��A^�/A]�PA\��A[��AY�mAWdZAU;dASS�AR��AR  APjAL�AK+AJA�AHbAE33ACABJA@�A>~�A>��A>(�A=��A=�A<bA;S�A;
=A:ZA8��A8Q�A7�A6bNA5ƨA4Q�A2��A1��A0�A0VA0E�A01A/S�A.�uA-�hA*�A)K�A(��A'�7A&�+A&bA%�A"�RA!�7A VA��A�/A-AO�A�A  AO�A��A�AƨAbA�A=qA��A�uAl�A
ZA	|�A��AĜAffA�A��AƨA�7A"�A�jA�A=qA`BA�HAM�A-A�A�TA�PA33A �A ~�A 9X@�\)@��@�@��@�r�@�"�@�@�r�@�"�@�-@��@��@�`B@���@�r�@�F@�\@���@��;@ꟾ@�Q�@�-@�&�@�P@��@�V@��@��@��@�h@�x�@�"�@�
=@�ȴ@�V@׶F@ԃ@҇+@ёh@�1'@���@ͩ�@̼j@̃@�I�@� �@� �@��@�  @˶F@˾w@�(�@�r�@�%@��@̴9@���@���@�^5@���@�`B@���@�1@��y@�n�@�/@�I�@���@�"�@��y@�=q@���@���@���@�
=@��@�@�@�5?@��7@���@��@��F@��P@�K�@��@��!@��!@��\@�~�@�ff@�V@�M�@�M�@�-@�x�@�%@�Ĝ@�z�@��m@��@�E�@���@��9@��@�K�@���@��@�@��H@���@��u@��@�z�@�r�@�(�@��w@�o@���@��R@���@��#@���@���@�G�@���@� �@�n�@���@�v�@���@��@���@�|�@���@�5?@�@�G�@��@��y@�^5@�5?@���@�=q@�M�@�5?@��T@���@��-@��h@�`B@�&�@���@��@�j@�(�@��w@�dZ@�ȴ@��R@��!@��+@�V@��@�@�p�@�&�@��@�(�@���@�\)@�33@�ȴ@�v�@�v�@���@�ȴ@��R@���@�ȴ@��R@���@��\@�-@��h@��@�X@��@��@���@�I�@�1@��
@��@�
=@��H@�ȴ@�ȴ@�V@�{@��@���@��7@�x�@�G�@��@��@���@���@�Z@���@���@���@���@�@��-@�x�@�hs@�hs@�?}@���@���@�Q�@��@��@�+@���@���@�^5@�M�@�-@�@�$�@�M�@�E�@���@���@���@���@�hs@�/@�(�@�  @��
@�|�@�t�@�dZ@�;d@�;d@�;d@�+@�o@��H@�~�@�ff@�V@�M�@�E�@�M�G�O�@�A @}�^@q�=@gH�@]�@V��@Nff@E�@@��@9��@4��@.�s@(�[@&_@ ֡@��@!�@�K@�U@1'@�.111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BB��B��B�B�fB�B��B��BDBoB�B�B�B�B&�B,B0!B5?B;dBA�BE�BH�BJ�BK�BVB^5BhsBl�Bn�Bn�Bn�Br�Bt�By�B}�B}�B�B� B� B�B�+B�JB��B��B��B��B��B��B�bB�=B�Bx�Bm�BaHBP�B?}B2-B(�B�BB��B�B�fB�BȴB�FB�B��B��B��B�uB�JB}�Bv�Br�Bs�B{�Bz�By�Bv�Br�Bn�BgmBcTBZBJ�BF�B=qB1'B-B#�BhB
�B
�/B
��B
ɺB
�RB
�B
��B
��B
��B
�B
k�B
[#B
N�B
F�B
6FB
&�B
�B	��B	�B	ȴB	�}B	�9B	�B	��B	��B	��B	��B	�oB	{�B	hsB	T�B	M�B	F�B	7LB	!�B	hB		7B��B�fB�B��B��B�B��B��B�B�B�yB�TB�HB�#B��B��BɺBǮBB�wB�LB�3B�B�B�B�B��B��B��B��B�uB�oB�hB�VB�JB�DB�1B�B� Bu�Bo�Bl�BiyBaHBZBXBVBVBR�BQ�BQ�BP�BO�BS�BXBZB\)B\)B[#B\)B[#B\)B\)B]/B\)BZB[#B^5B]/B]/B]/B]/B]/B_;B^5B\)B^5B_;B]/B\)BZBZBYBXBYBW
BVBVBQ�BVBdZBdZBe`Be`Be`Be`BdZB`BB_;B`BB]/BhsBk�BjBjBiyBgmBgmBffBdZB`BBYBe`BjBq�BjBe`BaHBbNBe`BhsBn�Bp�Br�Bt�Bu�Bu�Bv�B|�B~�B�B�7B�bB��B��B��B�B�B�B�'B�3B�LB�^B�dB�wBBÖBĜBĜBƨBȴB��B��B��B��B��B��B�B�)B�NB�`B�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	B	DB	\B	\B	\B	\B	oB	�B	�B	�B	#�B	#�B	%�B	'�B	(�B	)�B	(�B	(�B	'�B	&�B	&�B	)�B	+B	&�B	.B	'�B	%�B	%�B	'�B	2-B	7LB	9XB	>wB	>wB	<jB	<jB	=qB	>wB	=qB	>wB	?}B	A�B	A�B	A�B	E�B	H�B	J�B	K�B	L�B	L�B	M�B	N�B	P�B	Q�B	R�B	T�B	XB	\)B	^5B	_;B	_;B	`BB	aHB	bNB	cTB	dZB	cTB	dZB	e`B	e`B	gmB	hsB	jB	m�B	p�B	r�B	u�B	w�B	y�B	z�B	{�B	}�B	~�B	�B	�B	�7B	�7B	�DB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�LB	�LB	�RB	�dB	�qB	�}B	��B	��B	ÖB	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�/B	�)B	�)B	��B	��B
�B
FB
 'B
+B
33B
<PB
A;B
G�B
L�B
R�B
XEB
Z�B
`�B
b�B
gB
l�B
q'B
s�B
y$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�,B�B��B��B�B�9B�KB��B�B� B�^BެB��B�B�?B�B
�B�B�B�B�B,B$JB(cB-�B3�B9�B=�B@�BCBDBNDBVuB`�Bd�Bf�Bf�Bf�Bj�Bl�BrBv2Bv2ByDBx>Bx>BzJBiB��B��B��B��B��B��B��B��B�|BzKBqBe�BY�BI(B7�B*rB!<B�B�OB�B��BޱB�PB�B��B�]B�?B�B��B��B��BvFBoBkBl	Bt:Bs4Br.BoBkBf�B_�B[�BRrBCB>�B5�B)B%fB0B	�B
��B
ՍB
�KB
�B
��B
�uB
�RB
�B
��B
}�B
c�B
S�B
GAB
?B
.�B
TB
�B	�7B	шB	�&B	��B	��B	��B	�3B	�B	�!B	�B	��B	t_B	`�B	MxB	FNB	?#B	/�B	IB		�B	�B�\B��B�&B�hB�EB�B�>B�QB�2B�B��B��B��BӧB�}B�^B�?B�4B�B��B��B��B��B��B��B��B�xB�eB�GB�GB��B��B��B��B��B��B��Bz�Bx�BnPBh+BeBbBY�BR�BP�BN�BN�BK�BJ|BJ}BIvBHpBL�BP�BR�BT�BT�BS�BT�BS�BT�BT�BU�BT�BR�BS�BV�BU�BU�BU�BU�BU�BW�BV�BT�BV�BW�BU�BT�BR�BR�BQ�BP�BQ�BO�BN�BN�BJBN�B\�B\�B]�B]�B]�B]�B\�BX�BW�BX�BU�BaBdBcBcBbB` B` B^�B\�BX�BQ�B]�BcBj=BcB]�BY�BZ�B]�BaBg,Bi8BkDBmPBnWBnWBo]Bu�Bw�B}�B��B��B�B�OB��B��B��B��B��B��B��B��B��B�B�B�&B�,B�,B�8B�DB�QB�iBˁB͍B͍B͍BѦBԸB��B��B��B�B�B�,B�8B�>B�DB�JB�QB�WB�]B�WB�bB��B��B��B��B��B	�B	�B	�B	�B	�B	
�B	B	&B	8B	cB	cB	oB	 {B	!�B	"�B	!�B	!�B	 |B	uB	uB	"�B	#�B	uB	&�B	 |B	oB	oB	 |B	*�B	/�B	1�B	7B	7B	4�B	4�B	5�B	7B	5�B	7B	8B	:B	:B	:B	>-B	A?B	CLB	DRB	EXB	EXB	F^B	GcB	IoB	JvB	K|B	M�B	P�B	T�B	V�B	W�B	W�B	X�B	Y�B	Z�B	[�B	\�B	[�B	\�B	]�B	]�B	_�B	`�B	cB	fB	i-B	k9B	nLB	pXB	rcB	siB	toB	v|B	w�B	y�B	|�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�EB	�KB	�KB	�]B	�cB	�iB	�vB	�vB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�FB	�FB	�FB	�LB	�]B	�iB	�iB	�pB	�cB	�cB	�iB	�iB	�pB	�|B	�|B	͂B	ΈB	ΈB	ώB	ДB	ДB	ДB	ДB	ћB	ҡB	ԭB	ԭB	ճB	ճB	ճB	ԭG�O�B	�MB	�B
 JB
�B
�B
#�B
+�B
4�B
9�B
@HB
EMB
K	B
P�B
SnB
Y*B
[PB
_�B
eYB
i�B
lB
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144182022020411441820220204114418  AO  ARCAADJP                                                                    20200619170910    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170910  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170910  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114418  IP                  G�O�G�O�G�O�                