CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:54Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170854  20220204114411  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؄�oP�1   @؄��O��@7P��
=q�c���$�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B ffB*  B/33B8  B?��BH  BP  BX��B_33Bh  Bp  Bw��B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  Dy�D  D� D  D� D  D� D  Dy�D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�D�+�D�`�D�� D���D��D�mD���D���D�'\D�\{D���Dǋ3D��D�V�Dړ�D���D�#�D�PRD�3D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@��
@��
A�A=�A]�A}�A���A���A���A�A���A���A���A���Bz�Bz�Bz�B�GB)z�B.�B7z�B?{BGz�BOz�BXG�B^�Bgz�Boz�Bw{Bz�B��qB��qB��B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBÊ>BǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C�C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D�HDqHD��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��DqHD��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��DqHD�HDw�D��DqHD��Dw�D��Dw�D��Dw�D��DqHD��D~D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0�D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW�DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt�HDy��D�'\D�\�D���D�ؤD��D�h�D���D�ȤD�#3D�XRD��\DǇ
D��D�R�Dڏ�D���D��D�L)D�
D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�bNA�bNA�ZA�C�A�  AԴ9Aԥ�Aԥ�Aԥ�Aԥ�Aԩ�AԮA԰!A԰!AԴ9AԴ9AԮA�x�A��A�A�AГuA��/Aʙ�AȅA�bAǙ�AđhAð!A�1A���A���A���A�ĜA��#A���A�Q�A��9A��A�$�A�&�A�bA�=qA�(�A�-A��DA��;A��A��-A��TA��FA��uA�{A�JA�%A��jA���A�|�A��A�t�A�C�A��A�=qA��A��uA��
A�XA�A�A�A�dZA��9A���A�=qA���A��A���A�~�A�`BA���A��A�E�A���A�?}A�A��wA��\A�A�9XA�JA�C�A��FA�\)A��mA��DA�FA}�hA{;dAzVAyt�Aw�^Av��Au��AudZAt�Ar��Aq&�Ap��Ap�DAp5?Ao��Ao\)An1'Am%AlQ�Ak�hAkoAjQ�Aip�Ahz�Af�!Ae�
Ae��AeC�Ad�Ad��AdQ�AcO�Aa��A`1A_��A_XA]�
A]AYt�AWƨAW��AW�AW
=AU��AU��AU�ATQ�AS��AS\)AR��AQp�APJAO&�AN��AMC�AKdZAJI�AI�7AH(�AFI�AE�AC�PABĜABM�A@��A?�A?�7A?7LA=dZA<v�A;�A;7LA:��A9\)A8��A8bNA81A7��A7/A6ZA5�A4�A3oA1|�A/��A.ĜA.{A-l�A-"�A,��A+�A+�A*��A)\)A(��A(z�A(JA'XA&�A&M�A%�A$��A$ȴA$��A$r�A#�A"�A�FA�A��A&�A(�A��AoAG�A��AQ�A��A"�A5?Av�A�Ap�AS�A;dA��A��A�FAbNA�;A��A?}A	XAG�AG�A�A �Ap�A �HA 9X@�t�@��7@�j@�"�@�v�@�O�@���@��@�^5@�Z@��m@�@�@��@��@�@�  @���@�X@�S�@���@��#@�p�@���@�l�@���@��/@�Z@�ff@���@ߍP@��@�1@���@�v�@��T@�%@�C�@�
=@�@ա�@�|�@мj@ϕ�@Η�@���@��;@�+@�@ȼj@�
=@�@ċD@�l�@�o@��y@�ȴ@���@�ȴ@���@�$�@��@�1@�
=@�5?@�?}@��w@��H@�V@���@��@�v�@�p�@���@��/@��D@�K�@��!@�E�@�x�@�hs@�7L@�Ĝ@�t�@�~�@��^@�p�@��@��`@�r�@��@��@�M�@��@���@�?}@��@�Z@��@�5?@�hs@�9X@�t�@�v�@���@�O�@�&�@�V@��/@�z�@��w@�"�@���@��R@���@�~�@��@��-@��@�`B@�?}@���@�V@��`@�Ĝ@��j@�Ĝ@���@�Ĝ@�z�@��@�\)@��@�\)@�+@��@���@��+@�n�@�^5@�{@���@���@���@���@��9@�1'@��w@�"�@��@���@���@�{@�=q@�V@��@��@��@���@�@�G�@���@��/@�Q�@��m@���@�dZ@�l�@�33@�ȴ@�V@�$�@�-@���@�/@�`B@���@�@��^@�@���@��@�hs@�7L@��@���@�r�@�(�@��@��F@�"�@�o@�o@�"�@�\)@�ȴ@�^5@���@��@��@��@�5?@���@��@���@�j@�bN@���@�%@��/@��`@�Ĝ@�j@�1'@�  @���@��@��
@��@���@�|�@�dZ@�l�@���@�\)@���@��!@�v�@�J@�@�-@�V@�^5@��T@��@��@�%@�%@��`@���@���@��D@�1@��@��
@���@��w@���@���@��@�t�@�dZ@�33@�"�@��]@��F@w��@o�@d��@^H�@Ue,@KdZ@D~(@=:�@6�6@0��@+y�@&.�@!�7@@@C-@ݘ@��@�@7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�`BA�bNA�bNA�ZA�C�A�  AԴ9Aԥ�Aԥ�Aԥ�Aԥ�Aԩ�AԮA԰!A԰!AԴ9AԴ9AԮA�x�A��A�A�AГuA��/Aʙ�AȅA�bAǙ�AđhAð!A�1A���A���A���A�ĜA��#A���A�Q�A��9A��A�$�A�&�A�bA�=qA�(�A�-A��DA��;A��A��-A��TA��FA��uA�{A�JA�%A��jA���A�|�A��A�t�A�C�A��A�=qA��A��uA��
A�XA�A�A�A�dZA��9A���A�=qA���A��A���A�~�A�`BA���A��A�E�A���A�?}A�A��wA��\A�A�9XA�JA�C�A��FA�\)A��mA��DA�FA}�hA{;dAzVAyt�Aw�^Av��Au��AudZAt�Ar��Aq&�Ap��Ap�DAp5?Ao��Ao\)An1'Am%AlQ�Ak�hAkoAjQ�Aip�Ahz�Af�!Ae�
Ae��AeC�Ad�Ad��AdQ�AcO�Aa��A`1A_��A_XA]�
A]AYt�AWƨAW��AW�AW
=AU��AU��AU�ATQ�AS��AS\)AR��AQp�APJAO&�AN��AMC�AKdZAJI�AI�7AH(�AFI�AE�AC�PABĜABM�A@��A?�A?�7A?7LA=dZA<v�A;�A;7LA:��A9\)A8��A8bNA81A7��A7/A6ZA5�A4�A3oA1|�A/��A.ĜA.{A-l�A-"�A,��A+�A+�A*��A)\)A(��A(z�A(JA'XA&�A&M�A%�A$��A$ȴA$��A$r�A#�A"�A�FA�A��A&�A(�A��AoAG�A��AQ�A��A"�A5?Av�A�Ap�AS�A;dA��A��A�FAbNA�;A��A?}A	XAG�AG�A�A �Ap�A �HA 9X@�t�@��7@�j@�"�@�v�@�O�@���@��@�^5@�Z@��m@�@�@��@��@�@�  @���@�X@�S�@���@��#@�p�@���@�l�@���@��/@�Z@�ff@���@ߍP@��@�1@���@�v�@��T@�%@�C�@�
=@�@ա�@�|�@мj@ϕ�@Η�@���@��;@�+@�@ȼj@�
=@�@ċD@�l�@�o@��y@�ȴ@���@�ȴ@���@�$�@��@�1@�
=@�5?@�?}@��w@��H@�V@���@��@�v�@�p�@���@��/@��D@�K�@��!@�E�@�x�@�hs@�7L@�Ĝ@�t�@�~�@��^@�p�@��@��`@�r�@��@��@�M�@��@���@�?}@��@�Z@��@�5?@�hs@�9X@�t�@�v�@���@�O�@�&�@�V@��/@�z�@��w@�"�@���@��R@���@�~�@��@��-@��@�`B@�?}@���@�V@��`@�Ĝ@��j@�Ĝ@���@�Ĝ@�z�@��@�\)@��@�\)@�+@��@���@��+@�n�@�^5@�{@���@���@���@���@��9@�1'@��w@�"�@��@���@���@�{@�=q@�V@��@��@��@���@�@�G�@���@��/@�Q�@��m@���@�dZ@�l�@�33@�ȴ@�V@�$�@�-@���@�/@�`B@���@�@��^@�@���@��@�hs@�7L@��@���@�r�@�(�@��@��F@�"�@�o@�o@�"�@�\)@�ȴ@�^5@���@��@��@��@�5?@���@��@���@�j@�bN@���@�%@��/@��`@�Ĝ@�j@�1'@�  @���@��@��
@��@���@�|�@�dZ@�l�@���@�\)@���@��!@�v�@�J@�@�-@�V@�^5@��T@��@��@�%@�%@��`@���@���@��D@�1@��@��
@���@��w@���@���@��@�t�@�dZ@�33G�O�@��]@��F@w��@o�@d��@^H�@Ue,@KdZ@D~(@=:�@6�6@0��@+y�@&.�@!�7@@@C-@ݘ@��@�@7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B�B�B�!B�?B��B�yB��BBoB�B�B(�BVBo�B�B�}B��B��BǮBB�wBŢBÖBĜB�qB�'B�B�B��B��B��B��B��B�uB��B��B�uB�JB�B{�Bo�Bk�BhsBbNBS�BC�B33B$�B�BbB	7B%B  B��B�B�B�BB�BǮB�dB�B��B�bBz�BW
BH�B33B"�BbB%BB
��B
�B
�fB
�ZB
�HB
��B
�RB
�B
��B
�uB
�PB
�DB
�B
� B
x�B
r�B
cTB
[#B
XB
M�B
D�B
?}B
;dB
7LB
.B
$�B
 �B
�B
�B
�B
�B
uB
JB
+B
B
  B	��B	��B	�B	�sB	�HB	�BB	�5B	�)B	�#B	�B	��B	��B	��B	�qB	�^B	�3B	�B	��B	�\B	�PB	�JB	�=B	�B	�B	~�B	z�B	u�B	s�B	p�B	k�B	`BB	ZB	T�B	M�B	C�B	=qB	8RB	33B	,B	$�B	�B	�B	oB	VB	B	B	B��B��B�B�B�B�sB�`B�TB�HB�BB�/B�)B�B��B��BɺBĜB�}B�qB�^B�RB�^B�-B�-B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B�PB�JB�=B�1B�B~�Bx�Bu�Bs�Br�Bo�Bl�BdZBaHB`BB_;B_;B^5B]/B[#BW
BS�BQ�BO�BL�BG�BD�B?}B=qB:^B<jB:^B9XB8RB8RB8RB8RB8RB6FB49B49B2-B2-B1'B1'B1'B1'B1'B1'B0!B0!B0!B0!B0!B1'B49B6FB:^B=qB<jB8RB6FB7LB8RB8RB;dB<jB>wB@�BD�BC�BA�BB�BB�BB�BF�BK�BQ�BQ�BQ�BR�BT�BXBZB]/B`BB`BB`BB`BB`BB`BB`BBbNBcTBe`BffBffBffBiyBk�Bm�Bo�Br�Br�Bu�Bu�Bu�Bu�By�By�B{�B}�B}�B~�B~�B�B�B�+B�+B�7B�=B�PB�bB�oB�{B�{B��B��B��B��B��B��B��B��B��B�'B�?B�LB�LB�RB�^B�jB��BĜBƨB��B��B��B��B��B��B�B�BB�ZB�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	+B	1B	
=B	
=B	JB	PB	\B	bB	uB	�B	�B	�B	�B	�B	�B	&�B	)�B	.B	8RB	<jB	?}B	H�B	L�B	O�B	O�B	S�B	YB	\)B	\)B	^5B	`BB	hsB	jB	k�B	l�B	o�B	s�B	v�B	u�B	y�B	}�B	~�B	� B	�B	�B	�B	�%B	�+B	�=B	�DB	�VB	�\B	�VB	�VB	�\B	�VB	�\B	�\B	�hB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�3B	�3B	�3B	�3B	�3B	�9B	�FB	�^B	�jB	�dB	�^B	�XB	�dB	�jB	��B	��B	B	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�B	�B	�:B	��B
 �B
)B
�B
jB
)�B
4B
=qB
CB
IlB
N�B
SuB
W�B
]�B
a�B
f�B
kQB
oOB
qAB
uZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B�B�,B��B�bB�B��B	UBgByB�BL�BfB��B�ZB�`B�`B��B�lB�UB��B�uB�{B�RB�	B��B��B��B��B��B��B�xB�ZB�rB�lB�[B�0B{ Br�Bf�BbnB_\BY8BJ�B:�B*!B�B�BRB (B�B��B��B�B�B�7B�B��B�\B�B��B�]Bq�BN
B?�B*6B�BhB
�+B
�B
��B
�B
�oB
�cB
�RB
��B
�_B
�"B
��B
��B
�aB
�UB
{*B
wB
o�B
i�B
ZhB
R8B
O%B
D�B
;�B
6�B
2|B
.dB
%-B
�B
�B
�B
�B
�B
�B

�B
fB	�GB	�/B	�B	�B	��B	�B	ߒB	�hB	�bB	�UB	�IB	�CB	�0B	�B	��B	��B	��B	��B	�WB	�&B	��B	��B	�wB	�qB	�dB	|GB	y4B	v"B	r	B	l�B	j�B	g�B	b�B	WmB	QHB	L*B	D�B	:�B	4�B	/�B	*bB	#7B	B	�B	�B		�B	�B�RB�EB�9B�B��B��B��B��BߩBܖBڊB�~B�xB�fB�`B�;B�$B�B��B��B��B��B��B��B��B�hB�hB�bB�VB�DB�>B�>B�2B�,B�B�B�B��B��B��B��B��B��B��B��B�}BqBzRBv;BpBmBj�Bi�Bf�Bc�B[�BX�BW�BVBVBUyBTsBRhBNOBK=BI2BG%BDB>�B;�B6�B4�B1�B3�B1�B0�B/�B/�B/�B/�B/�B-�B+�B+�B)wB)wB(qB(qB(qB(qB(qB(rB'lB'lB'lB'lB'lB(rB+�B-�B1�B4�B3�B/�B-�B.�B/�B/�B2�B3�B5�B7�B;�B:�B8�B9�B9�B9�B=�BCBI7BI7BI7BJ=BLIBO[BQhBTzBW�BW�BW�BW�BW�BW�BW�BY�BZ�B\�B]�B]�B]�B`�Bb�Bd�Bf�Bi�Bi�BmBmBmBmBq%Bq%Bs1Bu>Bu>BvDBvDBz\B{cB~uB~uB��B��B��B��B��B��B��B��B��B��B��B��B�B�1B�DB�DB�oB��B��B��B��B��B��B��B��B��B�B�B�B�%B�8B�DB�\BׇB۟B��B��B��B��B��B� B�B�B�B�B�B�1B�CB�bB�nB�tB	B	B	�B	�B	�B	�B	
�B	�B	�B	�B	�B	�B	�B	*B	!=B	%TB	/�B	3�B	6�B	?�B	DB	GB	GB	K6B	PUB	SfB	SfB	UrB	WB	_�B	a�B	b�B	c�B	f�B	j�B	nB	l�B	qB	u/B	v5B	w;B	yGB	{TB	|ZB	}`B	~fB	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�;B	�GB	�SB	�YB	�_B	�eB	�eB	�eB	�kB	�kB	�kB	�kB	�kB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�"B	�"B	�(B	�4B	�:B	�:B	�:B	�:B	�@B	�@B	�FG�O�B	�oB	�B	��B
]B
�B
�B
 �B
+7B
4�B
:DB
@�B
E�B
J�B
N�B
T�B
Y0B
]�B
b�B
fB
hqB
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144112022020411441120220204114411  AO  ARCAADJP                                                                    20200619170854    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170854  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170854  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114411  IP                  G�O�G�O�G�O�                