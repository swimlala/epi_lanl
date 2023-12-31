CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:12Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170912  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               PA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��傜1   @��5�L@7�1&��c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    PA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D��D�\)D���D��)D�$�D�]�D���D��HD�
D�V�D���D��=D��D�K3Dڡ�D�ؤD��D�_
D�3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A�Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D �HDw�D��Dw�D��Dw�D��Dw�D��Dw�D��DqHD��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+qHD+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;~D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[�D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Do~Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dy��D�
�D�X D���D�� D� �D�Y�D��qD��D��D�R�D���D��D��D�G
DڝqD��{D�fD�Z�D�
D�Ƹ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A�?}A�=qA�?}A�?}A�;dA�7LA�7LA��yA�5?AͬA�5?A��A��HA�  AǋDA�l�AœuA�JAăA�dZA�%A�JA���A��A�dZA�XA�A�A���A�-A�VA���A��HA���A�VA�"�A�oA�JA�  A���A���A�ffA�"�A�{A���A�1'A��!A��A��A��7A�O�A��A���A�"�A��/A�JA�/A��HA��A�n�A�r�A�?}A�VA��!A��\A�n�A�bA���A� �A�VA�E�A���A���A��hA��A���A���A���A��A���A�`BA�JA�^5A�7LA��^A��A�`BA��wA���A��A�n�A��7A�ZA�|�A���A��^A���A�+A�;dA��DA�A�A��A���A��DA��A��yA��PA���A�XA�x�A�M�A��A�VA���A�ZA���A�S�A�A�hA~jAz�jAw|�Au�;AtjAqXAp�jAp�Ap=qAo�An��Am��Aj��Aj1Ag�FAcG�A_;dA^VA]�mA\�9A[�^AZVAY\)AWG�AVQ�AU\)AT�jAT$�ASO�AR~�AP�uAOhsAM��AK�AK�7AJ�`AI��AF��AFQ�AD�HAC�#AB��ABjAA��A?�FA>jA<~�A:�\A933A7�;A6�+A5�A3��A2^5A1��A0bNA/A,�/A+C�A*��A)��A)XA(JA&bNA$��A$jA"��A��A��A�DA�A�-AO�AbNA{AXA�A|�A�AA��Av�A
=A5?A�A��A��A��A��A
�`A	�#A	C�A?}A~�AM�AI�A9XAƨA^5A��A �@��H@�1@�|�@�+@���@�-@�p�@�V@��@��@�S�@��@��D@��@�M�@�r�@�-@�`B@�Q�@��m@畁@���@�Ĝ@�@�  @�F@�;d@�{@�  @ߕ�@�"�@��@�1@�t�@�
=@�~�@��@�x�@�bN@�\)@��@���@�C�@��@�o@�r�@�t�@�o@ɡ�@��@���@�@��@��#@�@��T@��#@�hs@ēu@�(�@öF@���@�E�@�=q@�n�@§�@��^@�z�@��w@�o@���@��@��F@�ȴ@�ff@�Ĝ@���@���@�"�@�\)@��P@��F@�t�@��T@��j@���@�?}@�Ĝ@��9@��@�\)@�@�@�o@�+@�+@��\@�1'@��F@�|�@�9X@�X@��/@��`@��m@���@���@� �@��@��`@�hs@���@���@��@��7@��@�j@�I�@�(�@��m@���@�  @��;@��F@��@�33@�$�@���@��-@���@���@�x�@��@�&�@��@�Ĝ@�j@��@��F@�\)@��y@�v�@�-@��@���@�j@�1'@�A�@�33@�@��j@��@���@��j@���@�I�@���@���@�Z@�l�@��@��H@���@���@���@�V@�E�@�M�@�V@�M�@�^5@��y@�-@�`B@�G�@�V@�V@�%@���@��D@�1'@�1@��
@��w@�S�@���@��+@�=q@�{@���@��`@���@��D@�I�@� �@�ƨ@�t�@�t�@�o@��H@�ȴ@�"�@�@�M�@�^5@�-@�-@�J@��7@�Ĝ@��9@�I�@��;@�l�@�;d@�+@�o@��@���@�ff@�=q@���@��@���@��h@��@��@���@��`@��j@�j@�I�@�(�@�(�@��m@���@�|�@�dZ@���@��R@�~�@��@���@�?}@��@���@���@�r�@�Z@�A�@��@��
@�|�@�;d@��@�
=@��y@���@���@�n�@�-@�J@���@��T@�@��7@�X@�/@�,�@~xl@t��@lb@d �@\�@U�@Mf�@E��@?�q@9s�@1V@*~�@$�D@u%@��@�@�@��@	��@!-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�?}A�?}A�=qA�?}A�?}A�;dA�7LA�7LA��yA�5?AͬA�5?A��A��HA�  AǋDA�l�AœuA�JAăA�dZA�%A�JA���A��A�dZA�XA�A�A���A�-A�VA���A��HA���A�VA�"�A�oA�JA�  A���A���A�ffA�"�A�{A���A�1'A��!A��A��A��7A�O�A��A���A�"�A��/A�JA�/A��HA��A�n�A�r�A�?}A�VA��!A��\A�n�A�bA���A� �A�VA�E�A���A���A��hA��A���A���A���A��A���A�`BA�JA�^5A�7LA��^A��A�`BA��wA���A��A�n�A��7A�ZA�|�A���A��^A���A�+A�;dA��DA�A�A��A���A��DA��A��yA��PA���A�XA�x�A�M�A��A�VA���A�ZA���A�S�A�A�hA~jAz�jAw|�Au�;AtjAqXAp�jAp�Ap=qAo�An��Am��Aj��Aj1Ag�FAcG�A_;dA^VA]�mA\�9A[�^AZVAY\)AWG�AVQ�AU\)AT�jAT$�ASO�AR~�AP�uAOhsAM��AK�AK�7AJ�`AI��AF��AFQ�AD�HAC�#AB��ABjAA��A?�FA>jA<~�A:�\A933A7�;A6�+A5�A3��A2^5A1��A0bNA/A,�/A+C�A*��A)��A)XA(JA&bNA$��A$jA"��A��A��A�DA�A�-AO�AbNA{AXA�A|�A�AA��Av�A
=A5?A�A��A��A��A��A
�`A	�#A	C�A?}A~�AM�AI�A9XAƨA^5A��A �@��H@�1@�|�@�+@���@�-@�p�@�V@��@��@�S�@��@��D@��@�M�@�r�@�-@�`B@�Q�@��m@畁@���@�Ĝ@�@�  @�F@�;d@�{@�  @ߕ�@�"�@��@�1@�t�@�
=@�~�@��@�x�@�bN@�\)@��@���@�C�@��@�o@�r�@�t�@�o@ɡ�@��@���@�@��@��#@�@��T@��#@�hs@ēu@�(�@öF@���@�E�@�=q@�n�@§�@��^@�z�@��w@�o@���@��@��F@�ȴ@�ff@�Ĝ@���@���@�"�@�\)@��P@��F@�t�@��T@��j@���@�?}@�Ĝ@��9@��@�\)@�@�@�o@�+@�+@��\@�1'@��F@�|�@�9X@�X@��/@��`@��m@���@���@� �@��@��`@�hs@���@���@��@��7@��@�j@�I�@�(�@��m@���@�  @��;@��F@��@�33@�$�@���@��-@���@���@�x�@��@�&�@��@�Ĝ@�j@��@��F@�\)@��y@�v�@�-@��@���@�j@�1'@�A�@�33@�@��j@��@���@��j@���@�I�@���@���@�Z@�l�@��@��H@���@���@���@�V@�E�@�M�@�V@�M�@�^5@��y@�-@�`B@�G�@�V@�V@�%@���@��D@�1'@�1@��
@��w@�S�@���@��+@�=q@�{@���@��`@���@��D@�I�@� �@�ƨ@�t�@�t�@�o@��H@�ȴ@�"�@�@�M�@�^5@�-@�-@�J@��7@�Ĝ@��9@�I�@��;@�l�@�;d@�+@�o@��@���@�ff@�=q@���@��@���@��h@��@��@���@��`@��j@�j@�I�@�(�@�(�@��m@���@�|�@�dZ@���@��R@�~�@��@���@�?}@��@���@���@�r�@�Z@�A�@��@��
@�|�@�;d@��@�
=@��y@���@���@�n�@�-@�J@���@��T@�@��7@�XG�O�@�,�@~xl@t��@lb@d �@\�@U�@Mf�@E��@?�q@9s�@1V@*~�@$�D@u%@��@�@�@��@	��@!-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�sB
�mB
�sB
�HB
�B	7B�B-B/B49B>wBP�BgmB� B�JB��B�3B�B�5B�#B�;B�sB��B��BBBBB%BDBuB�B$�B33BC�BL�BO�BT�BW
BW
B]/BbNBhsBiyBk�Bt�Bm�BdZBZBJ�B}�B�PBz�BR�B>wBS�BN�B49B9XBL�BP�BO�B^5Be`Bp�Bk�Bp�Bz�Bv�BVB?}B7LB,B/B.B#�B�BbB  B�B�B�BB��BÖB�?B��B��B�\B�Bv�Bk�BE�B1'B)�B!�B\BB
��B
�#B
�^B
��B
��B
�B
v�B
p�B
iyB
[#B
=qB
5?B
-B
{B
B	��B	��B	��B	�B	�B	�B	�B	�fB	�ZB	��B	ÖB	�jB	�bB	bNB	o�B	v�B	s�B	l�B	aHB	\)B	S�B	L�B	G�B	B�B	@�B	:^B	5?B	,B	!�B	�B	hB	JB		7B	B��B�B�B�`B�HB�/B�B��BB�qB�-B�'B�B��B��B��B��B�oB�VB�=B�+B�B�B� B|�B{�Bv�Bt�Bp�Bo�BbNB\)BYBW
BT�BR�BQ�BP�BP�BQ�BO�BN�BM�BK�BJ�BI�BH�BF�BE�BC�BB�BA�B>wB;dB<jB;dB;dB:^B9XB9XB9XB:^B:^B6FB;dB;dB;dB;dB;dB<jB<jB;dB<jB;dB;dB;dB:^B9XB:^B>wB6FB6FB9XB:^B:^B:^BB�BH�BH�BH�BI�BJ�BK�BJ�BJ�BI�BI�BG�BF�BG�BI�BM�BN�BO�BVBW
BT�BR�BO�BK�BG�BE�BF�BC�BD�BI�BM�BN�BP�BR�BS�BYB[#B[#B\)B\)B`BBhsBs�Bx�B}�B}�B|�B|�B|�B|�B|�B� B�B� B}�B}�B�DB�{B��B��B��B��B��B��B��B��B�B�!B�RB�wBÖBƨBȴBɺB��BƨBǮB��B��B�NB�fB�mB�sB�fB�mB�yB�B��B	B	�B	'�B	+B	/B	/B	/B	0!B	1'B	49B	5?B	9XB	9XB	9XB	8RB	7LB	7LB	9XB	=qB	=qB	>wB	?}B	C�B	E�B	F�B	G�B	H�B	H�B	I�B	H�B	H�B	G�B	F�B	E�B	G�B	K�B	L�B	M�B	P�B	O�B	R�B	XB	YB	XB	XB	VB	T�B	P�B	Q�B	T�B	YB	YB	[#B	]/B	]/B	`BB	aHB	bNB	bNB	bNB	cTB	hsB	jB	hsB	iyB	n�B	o�B	p�B	r�B	t�B	w�B	x�B	y�B	y�B	|�B	~�B	�B	�B	�B	�B	�7B	�PB	�bB	�hB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�-B	�3B	�3B	�3B	�9B	�9B	�FB	�LB	�^B	�dB	�dB	�wB	�}B	�}B	��B	��B	��B	��B	B	B	B	ĜB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	��B	�;B	��B
�B
�B
# B
+�B
3�B
;B
B'B
G�B
OBB
T�B
ZkB
_�B
d�B
i*B
l�B
q�B
u�B
y>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�:B
�!B
��B
��B
��B
ٻB
�B�BB%}B'�B,�B6�BISB_�BxlB��B�B��B҄B֜BӊBעB��B�5B�SB�qB�~B��B��B��B�B�BB@B+�B;�BE/BH@BM_BOkBOkBU�BZ�B`�Ba�Bc�BmBe�B\�BRBC$BvTB��BsABKUB6�BL[BG<B,�B1�BE1BIHBHCBV�B]�BiBc�BiBsBBo+BNhB7�B/�B$oB'�B&{B>BB�B�jB�B��BخB�LB�B��B�lB�B��B{�Bo<Bc�B>B)�B"tBDB�B
��B
�WB
ӡB
��B
�qB
�	B
|�B
oNB
i)B
a�B
S�B
5�B
-�B
%�B
B	��B	�hB	�WB	�WB	�8B	�8B	�>B	� B	��B	��B	�XB	�(B	��B	��B	Z�B	h6B	o`B	lNB	e#B	Y�B	T�B	L�B	EgB	@IB	;*B	9B	2�B	-�B	$�B	iB	>B	
B	�B	�B��B�pB�EB�!B�B��B��BΧB�wB�4B�B��B��B��B�rB�lB�BB�*B�B��B��B�B{�Bz�Bx�Bu�Bt�BouBmhBiPBhJBZ�BT�BQ�BO�BM�BK�BJ�BI�BI�BJ�BH�BG�BF�BDwBCqBBjBAdB?YB>SB<GB;@B:;B7)B4B5B4B4B3B2B2B2B3B3B.�B4B4B4B4B4B5B5B4B5B4B4B4B3B2B3B7+B.�B.�B2B3B3B3B;DBAhBAhBAhBBnBCuBD{BCvBCvBBoBBoB@cB?]B@cBBoBF�BG�BH�BN�BO�BM�BK�BH�BD}B@dB>XB?^B<MB=SBBpBF�BG�BI�BK�BL�BQ�BS�BS�BT�BT�BX�Ba(BljBq�Bv�Bv�Bu�Bu�Bu�Bu�Bu�Bx�B{�Bx�Bv�Bv�B��B�.B�FB�SB�_B�YB�MB�YB��B��B��B��B�B�(B�GB�YB�eB�kB�rB�YB�_B�rBˢB��B�B�B�"B�B�B�(B�SB�B��B	XB	 �B	#�B	'�B	'�B	'�B	(�B	)�B	,�B	-�B	2B	2B	2B	0�B	/�B	/�B	2B	6B	6B	7!B	8'B	<@B	>LB	?RB	@XB	A^B	A^B	BdB	A^B	A^B	@XB	?RB	>LB	@XB	DqB	EwB	F}B	I�B	H�B	K�B	P�B	Q�B	P�B	P�B	N�B	M�B	I�B	J�B	M�B	Q�B	Q�B	S�B	U�B	U�B	X�B	Y�B	Z�B	Z�B	Z�B	[�B	aB	c(B	aB	b"B	gAB	hGB	iMB	kXB	mdB	pwB	q}B	r�B	r�B	u�B	w�B	y�B	{�B	{�B	}�B	��B	��B	�	B	�B	�	B	�B	�4B	�:B	�AB	�:B	�:B	�MB	�RB	�GB	�^B	�kB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	�B	�"B	�"B	�(B	�(B	�(B	�.B	�4B	�4B	�4B	�AB	�AB	�GB	�SB	�_B	�qB	�qB	�wB	ɉB	ʐB	ʐB	˖B	˖B	˖B	̜B	̜B	̜B	͢B	͢B	͢B	͢B	ϮB	ϮB	дB	ѻB	ѻB	��B	��B	��G�O�B	ޡB	��B	�.B
,B
'B
�B
$[B
,�B
4B
:�B
@hB
G�B
M�B
SB
XDB
]HB
a�B
eyB
j�B
nGB
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20200619170912    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170912  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170912  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                