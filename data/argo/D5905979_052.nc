CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:06Z creation      
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20200619170906  20220204114415  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               4A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؟����1   @؟l͖@5��G�{�c��x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    4A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds� Dy�D���D�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C�RC!�RC#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,qHD,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9�D:w�D:��D;w�D;��D<w�D<�D=w�D=��D>w�D>��D?w�D?��D@qHD@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG�DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg�Dh~Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds׮Dy{D���D� �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aƕ�AƍPA�n�A�%A��TA��;A���A�AŶFAŮA�t�A�C�A�{A�A�  A��;AĶFAĕ�A�;dA�
=A�bNA�1A�|�A��uA��A�A�bA�VA�l�A��yA���A��A��-A�x�A�z�A���A�A�A���A�-A�$�A���A��A���A��\A�%A�?}A�5?A��A��A�A�A���A�$�A�K�A���A��yA�n�A�1A���A�9XA��\A�;dA��A���A�|�A�t�A�(�A�hsA��+A��hA��A�v�A���A�A�A�x�A�ZA�E�A���A���A�~�A�=qA���A��
A���A���A�S�A�ȴA�/A��wA���A��\A��A�K�A��yA��HA��A�x�A���A�\)A�I�A��/A�33A��+A��A���A��A�ȴA���A��RA�+A��A|�A}dZAw�Au�Au+At  AsXArv�Aq��Ao��Al�yAi�-Af��AdM�Aa��Aa;dAa%A`�9A^E�AX�9AW�AVM�AT�HAQ��AO�AL�\AJ�9AH�\AG�;AG&�AFI�AD��AD�HAD��ADbNACt�AB�DAA�A@��A@VA@bA?�-A>1A=\)A<^5A9�A8$�A7�7A6��A4��A3/A/��A.VA,ĜA+�A*$�A&�!A$VA#��A#�A#x�A"�A"=qA"{A!�A ��A��A�mA�A&�A�/AbNAdZA��AXA�/A�yAn�A��A�7A7LA��A�A
ȴA	A�yA�#A�AS�A"�A��AĜAbNA-A�#Ar�@�ȴ@���@�1'@�v�@���@�&�@�bN@�@�|�@�l�@�S�@�;d@�=q@�Ĝ@�ȴ@�
=@�\)@�+@��@�+@�ff@��T@�&�@�?}@�o@�~�@ٺ^@��/@��@ץ�@�@��@�G�@�j@�b@�
=@���@���@Ұ!@�^5@��@Ѳ-@���@��@ϝ�@Ͼw@�33@��@�n�@�%@�^5@�ƨ@� �@��@�J@���@�7L@��@�E�@��T@���@�Ĝ@�l�@�-@���@�?}@�I�@��F@�^5@�&�@��`@���@�  @�l�@�=q@�Q�@�\)@�+@��@�n�@�hs@�7L@�%@�G�@��7@�`B@�b@�\)@��H@��y@��@���@��!@��+@�ff@�V@�5?@�{@�@��T@��D@��@��w@��
@��@���@���@���@���@�;d@�
=@��@�"�@�"�@�+@�+@��@��@��@��@���@���@��\@��+@�ff@�n�@�^5@�5?@�-@�-@��@�@���@��T@���@���@��@�O�@�V@�bN@�(�@���@�Q�@���@��;@�ȴ@�v�@��^@���@�33@�ȴ@��\@�v�@�M�@�E�@�=q@��@��@���@��7@�p�@�X@�/@���@�ƨ@�K�@��H@���@�ff@��@���@�&�@�V@��@�%@��`@��9@���@�C�@�
=@��@��@��^@��^@��h@���@��@���@�z�@���@�bN@��w@�dZ@�O�@�V@���@��`@��@���@�Ĝ@���@��D@��D@���@���@���@��9@��j@��j@���@�j@�Z@�I�@�I�@�1'@�b@�b@�1@�b@�b@��@��@��@��@� �@��P@�
=@�o@�o@�
=@�"�@�"�@�"�@�+@�+@�"�@��H@�
=@�K�@�K�@�S�@�\)@�\)@��\@�V@�V@�~�@�V@�$�@�5?@�E�@�E�@�E�@�E�@�5?@�5?@�5?@�5?@�=q@�M�@��@�-@��#@�p�@��@��-@��^@��^@���@���@���@���@��^@�/@�Ĝ@���@�z�@�Q�@�1'@�(�@�1'@�J�@�!@~\�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444  Aƕ�AƍPA�n�A�%A��TA��;A���A�AŶFAŮA�t�A�C�A�{A�A�  A��;AĶFAĕ�A�;dA�
=A�bNA�1A�|�A��uA��A�A�bA�VA�l�A��yA���A��A��-A�x�A�z�A���A�A�A���A�-A�$�A���A��A���A��\A�%A�?}A�5?A��A��A�A�A���A�$�A�K�A���A��yA�n�A�1A���A�9XA��\A�;dA��A���A�|�A�t�A�(�A�hsA��+A��hA��A�v�A���A�A�A�x�A�ZA�E�A���A���A�~�A�=qA���A��
A���A���A�S�A�ȴA�/A��wA���A��\A��A�K�A��yA��HA��A�x�A���A�\)A�I�A��/A�33A��+A��A���A��A�ȴA���A��RA�+A��A|�A}dZAw�Au�Au+At  AsXArv�Aq��Ao��Al�yAi�-Af��AdM�Aa��Aa;dAa%A`�9A^E�AX�9AW�AVM�AT�HAQ��AO�AL�\AJ�9AH�\AG�;AG&�AFI�AD��AD�HAD��ADbNACt�AB�DAA�A@��A@VA@bA?�-A>1A=\)A<^5A9�A8$�A7�7A6��A4��A3/A/��A.VA,ĜA+�A*$�A&�!A$VA#��A#�A#x�A"�A"=qA"{A!�A ��A��A�mA�A&�A�/AbNAdZA��AXA�/A�yAn�A��A�7A7LA��A�A
ȴA	A�yA�#A�AS�A"�A��AĜAbNA-A�#Ar�@�ȴ@���@�1'@�v�@���@�&�@�bN@�@�|�@�l�@�S�@�;d@�=q@�Ĝ@�ȴ@�
=@�\)@�+@��@�+@�ff@��T@�&�@�?}@�o@�~�@ٺ^@��/@��@ץ�@�@��@�G�@�j@�b@�
=@���@���@Ұ!@�^5@��@Ѳ-@���@��@ϝ�@Ͼw@�33@��@�n�@�%@�^5@�ƨ@� �@��@�J@���@�7L@��@�E�@��T@���@�Ĝ@�l�@�-@���@�?}@�I�@��F@�^5@�&�@��`@���@�  @�l�@�=q@�Q�@�\)@�+@��@�n�@�hs@�7L@�%@�G�@��7@�`B@�b@�\)@��H@��y@��@���@��!@��+@�ff@�V@�5?@�{@�@��T@��D@��@��w@��
@��@���@���@���@���@�;d@�
=@��@�"�@�"�@�+@�+@��@��@��@��@���@���@��\@��+@�ff@�n�@�^5@�5?@�-@�-@��@�@���@��T@���@���@��@�O�@�V@�bN@�(�@���@�Q�@���@��;@�ȴ@�v�@��^@���@�33@�ȴ@��\@�v�@�M�@�E�@�=q@��@��@���@��7@�p�@�X@�/@���@�ƨ@�K�@��H@���@�ff@��@���@�&�@�V@��@�%@��`@��9@���@�C�@�
=@��@��@��^@��^@��h@���@��@���@�z�@���@�bN@��w@�dZ@�O�@�V@���@��`@��@���@�Ĝ@���@��D@��D@���@���@���@��9@��j@��j@���@�j@�Z@�I�@�I�@�1'@�b@�b@�1@�b@�b@��@��@��@��@� �@��P@�
=@�o@�o@�
=@�"�@�"�@�"�@�+@�+@�"�@��H@�
=@�K�@�K�@�S�@�\)@�\)@��\@�V@�V@�~�@�V@�$�@�5?@�E�@�E�@�E�@�E�@�5?@�5?@�5?@�5?@�=q@�M�@��@�-@��#@�p�@��@��-@��^@��^@���@���@���@���@��^@�/@�Ĝ@���@�z�@�Q�@�1'@�(�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�B
�B
�B
�BoBoB�B!�B-B5?B;dBJ�BVBaHBiyBn�Bt�By�B}�B�1B�B�qBƨB��B�B��B��BoB#�B2-B6FB9XB>wBN�B^5Be`BgmBgmBo�Bl�Bl�Bl�Bm�Bk�Bk�BiyBhsBgmBbNBbNBaHB^5B_;B\)BYBP�BK�BH�BE�BC�B?}B>wB=qB;dB9XB:^B8RB/B#�B�B�B�BJB�B�`B�B��B�'B�{B�7B�B� B|�B{�Bx�Br�BjBT�BA�B=qB<jB;dB:^B5?B&�BVBB
��B
�B
�TB
ɺB
�}B
�FB
�B
�B
�B
��B
��B
��B
�PB
gmB
[#B
N�B
-B
bB
JB
B	��B	�B	�mB	��B	ŢB	�B	��B	~�B	q�B	n�B	o�B	l�B	cTB	2-B	 �B	�B	�B	B��B�B�NB�B�B��B��B��B��B��B��BǮBÖB�dB�?B�'B�B�B��B��B��B��B�JB�+B�B{�Bt�Br�Bo�Bm�BhsBhsBffBcTBbNBaHBaHBbNB_;B]/B]/B[#BYBT�BR�BN�BL�BJ�BG�BF�BD�BD�BC�BA�BA�BA�BA�BA�BB�BC�BC�BD�BJ�BM�BO�BQ�BS�B\)BZBYBVBT�BhsBm�Bm�BjBhsBhsBiyBiyBiyBjBjBjBk�BjBl�Bq�Br�Bq�Bs�Bs�Br�Bq�Bo�Bq�BiyBhsBgmBe`Be`BdZBcTBcTBbNBbNB`BB`BB_;B`BB_;B_;B^5B^5B]/B\)BZBZBZBXBYBYB]/B]/BffBe`BffBgmBhsBl�Bq�Bq�Br�Bu�B{�B� B�B�B�1B�7B�\B�oB�uB�{B��B��B��B��B�B�B�B�XB�^B�wB��BĜBɺB��B��B�
B�B�B�B�B�B�B�B�B�B�B�B�B�HB�ZB�`B�`B�mB�mB�mB�mB�mB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	1B	1B	B	B	oB	%�B	,B	.B	49B	7LB	F�B	H�B	I�B	I�B	J�B	K�B	K�B	M�B	P�B	S�B	XB	XB	ZB	ZB	]/B	cTB	ffB	iyB	jB	m�B	o�B	r�B	t�B	t�B	t�B	t�B	u�B	v�B	}�B	�B	�B	�B	�=B	�JB	�JB	�PB	�oB	�hB	�uB	��B	�{B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�-B	�-B	�-B	�-B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�?B	�FB	�LB	�LB	�LB	�LB	�LB	�LB	�LB	�LB	�FB	�FB	�^B	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�dB	�dB	�jB	�}B	�qB	�^B	�dB	�^B	�^B	�^B	B	ÖB	ÖB	��B	ÖB	ŢB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ĜB	ŢB	ŢB	ƨB	ȴB	ɺB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�:B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444  B
��B
��B
�B
HB
HBfB�B$�B-B3:BB�BM�BYBaMBfkBl�Bq�Bu�B�B��B�@B�vB��B�KB�B��B
9B�B)�B.B1 B6?BF�BU�B]'B_4B_4BgeBdRBdRBdRBeXBcLBcLBaAB`;B_5BZBZBYBU�BWBS�BP�BH�BC�B@B=nB;bB7IB6CB5>B31B1%B2+B0B&�B�BcB]B]BB�B�3B��BƮB��B�UB�B|�Bw�Bt�Bs�Bp�Bj�Bb\BL�B9iB5QB4JB3DB2>B- B�B:B
��B
�B
�~B
�;B
��B
�fB
�0B
��B
��B
��B
��B
��B
�sB
�=B
_]B
SB
F�B
%B
XB
@B	��B	��B	�B	�fB	��B	��B	� B	��B	v�B	i�B	f�B	g�B	d�B	[WB	*4B	�B	�B	�B�B��B�B�[B�+B�B�B�B��B��B��B��B��B��B�uB�PB�8B� B�B��B��B��B��B�_B@B}4Bs�Bl�Bj�Bg�Be�B`�B`�B^B[mBZgBYbBYbBZhBWUBUIBUIBS=BQ2BMBKBF�BD�BB�B?�B>�B<�B<�B;�B9�B9�B9�B9�B9�B:�B;�B;�B<�BB�BE�BG�BJ
BLBTGBR;BQ5BN"BMB`�Be�Be�Bb�B`�B`�Ba�Ba�Ba�Bb�Bb�Bb�Bc�Bb�Bd�Bi�Bj�Bi�Bk�Bk�Bj�Bi�Bg�Bi�Ba�B`�B_�B]�B]�B\{B[uB[uBZoBZoBXcBXcBW\BXcBW\BW\BVVBVVBUPBTJBR?BR?BR?BP2BQ9BQ9BUQBUQB^�B]�B^�B_�B`�Bd�Bi�Bi�Bj�Bm�BtBx!Bz-B|:B�QB�WB�|B��B��B��B��B��B��B�B�-B�-B�:B�vB�|B��B��B��B��B��B�B�'B�-B�-B�-B�-B�-B�4B�4B�4B�:B�:B�:B�:B�dB�vB�|B�|B߉B߉B߉B߉B߉B�B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B� B�&B	 KB	 KB�9B�9B	
�B	�B	$ B	&,B	,QB	/dB	>�B	@�B	A�B	A�B	B�B	C�B	C�B	E�B	H�B	LB	P&B	P&B	R3B	R3B	UEB	[iB	^{B	a�B	b�B	e�B	g�B	j�B	l�B	l�B	l�B	l�B	m�B	n�B	vB	yB	{&B	|-B	�PB	�]B	�]B	�cB	��B	�{B	��B	��B	��B	��B	��B	��B	�B	�&B	�&B	�-B	�-B	�3B	�3B	�9B	�>B	�>B	�>B	�>B	�>B	�9B	�>B	�>B	�DB	�JB	�JB	�PB	�PB	�WB	�]B	�]B	�]B	�]B	�]B	�]B	�]B	�]B	�WB	�WB	�oB	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�uB	�uB	�{B	��B	��B	�oB	�uB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144152022020411441520220204114416  AO  ARCAADJP                                                                    20200619170906    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170906  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170906  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                