CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:04Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170904  20220204114415  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               -A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؖ]'�1   @ؖ]��MV@7�E����c��/��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    -A   B   B   @���@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D2��D3y�D4  D4� D5  D5� D6fD6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyS3D� RD�W
D��HD��HD�  D�J�D��
D���D�( D�Y�D��{D��)D�\D�YHDڠ�D���D�3D�^D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@��
@��
A�A=�A]�A|Q�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/{B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��B�#�B��>B��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qBߊ>B�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.�D/w�D/��D0w�D0��D1w�D1��D2w�D2�HD3qHD3��D4w�D4��D5w�D5�D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt�DyJ�D�)D�R�D��D��D��D�FfD���D�θD�#�D�U�D��RD�� D�3D�UDڜ�D��D�
D�Y�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�AǁAǉ7AǑhAǓuAǕ�AǕ�AǗ�AǗ�AǗ�AǙ�AǙ�AǛ�AǛ�AǛ�Aǝ�Aǝ�Aǝ�Aǝ�Aǟ�Aǟ�Aǡ�Aǡ�Aǣ�Aǡ�Aǡ�Aǡ�Aǡ�Aǡ�Aǣ�Aǥ�Aǧ�Aǧ�Aǧ�Aǩ�Aǩ�AǬAǧ�Aǟ�AǕ�A�A�^5A� �A���A��/A�l�A��HA��DA�G�A�JA�^5A��/A�jA��mA�=qA�n�A��yA��DA��jA���A�A��TA��wA��+A�ffA��DA�x�A� �A���A��A���A��9A�l�A�7LA�{A��FA��A��A�G�A��A��DA�33A��#A���A�E�A���A��mA�&�A�%A�A���A�VA��A�XA�%A��A���A���A��^A��hA��jA��A�`BA�  A��A�?}A�VA��;A�O�A��`A��FA�n�A���A�XA���A��hA�A��A�1'A�=qA��\A�;dA��#A� �A���A�K�A�`BA��A~jA{�Aw��Au��At��Atr�AtQ�Arz�An{Aj�RAg�Af��Af�+Ae�Ad�\Ac&�A`��A`1A_hsA^  AZ��AX��AW��AUVAS�wAR��AP1'AO"�ANbNAM�^AJ�AF�jAE��AF-AD�AC��AB1'AA��A@�A?ƨA>~�A=C�A<M�A;��A:�A8�yA7�7A6�uA5�A4bNA2n�A1��A1;dA0��A/��A,�\A+ƨA+��A+�A+�A)S�A(9XA({A'|�A&�A%�A"��A VA�RA�A  A?}A��AffA/AM�AƨA��AdZA/A�At�A��A9XA�A��A��A�;A/A{AhsA"�A	�
A	&�A�/A�9A�uA�A�yAbNA�FA�9A��A�AhsA �\A 9XA {@�~�@�Z@��H@�/@�J@���@�P@���@��/@���@�7@��@��T@�x�@��`@�@�b@�9X@�V@�j@�D@�Z@��@��@�7@�@���@�|�@�;d@�v�@���@��@�E�@أ�@��@�?}@��`@�1'@�@Ѻ^@���@�K�@�7L@�j@˶F@��@���@�Ĝ@�ƨ@ǅ@�K�@��@Ɵ�@�V@�=q@��T@��@å�@�@��@�b@�\)@��+@��@��y@��@�x�@�?}@�&�@��@���@��j@���@�5?@�9X@�
=@�^5@���@�V@�A�@�|�@��@�@��+@�@�Ĝ@���@�C�@��H@���@�ȴ@���@��`@�bN@�+@���@�~�@�-@�@��T@���@�G�@�/@�7L@�7L@�?}@�O�@�&�@�&�@�p�@�`B@�?}@�?}@�G�@�7L@�V@��j@�Q�@���@�n�@��-@��-@���@�@�p�@�G�@�%@��`@���@��@��@�I�@��;@�ƨ@�t�@��@�b@�1@��m@��
@��@�S�@��@�@��@��H@��y@��\@��\@�J@��#@���@�O�@���@���@��F@���@�l�@�33@�^5@�`B@�r�@��D@�b@�o@���@�&�@�r�@���@��
@���@��P@�
=@��R@�~�@�{@���@���@�V@�ff@�5?@��@���@��`@��D@��
@�33@�ȴ@��\@�J@���@�p�@��h@�p�@�x�@�X@�/@��`@�V@��@��j@�1'@�1@���@���@�;d@�@��H@�ȴ@��R@�^5@��@��@���@��-@���@��h@�x�@�`B@�?}@�V@���@�r�@�9X@��@��@�  @��;@���@�|�@�dZ@�S�@�;d@��@���@��R@��\@�^5@��@��-@��h@�x�@�V@��/@�Ĝ@�r�@���@��w@�dZ@�"�@�@��H@��+@��9@v�@m%F@e�7@\h�@T7�@N@F	@?��@:��@4��@.�@(��@$�U@!Y�@�Q@�@�@�@�a@ی111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�~�AǁAǉ7AǑhAǓuAǕ�AǕ�AǗ�AǗ�AǗ�AǙ�AǙ�AǛ�AǛ�AǛ�Aǝ�Aǝ�Aǝ�Aǝ�Aǟ�Aǟ�Aǡ�Aǡ�Aǣ�Aǡ�Aǡ�Aǡ�Aǡ�Aǡ�Aǣ�Aǥ�Aǧ�Aǧ�Aǧ�Aǩ�Aǩ�AǬAǧ�Aǟ�AǕ�A�A�^5A� �A���A��/A�l�A��HA��DA�G�A�JA�^5A��/A�jA��mA�=qA�n�A��yA��DA��jA���A�A��TA��wA��+A�ffA��DA�x�A� �A���A��A���A��9A�l�A�7LA�{A��FA��A��A�G�A��A��DA�33A��#A���A�E�A���A��mA�&�A�%A�A���A�VA��A�XA�%A��A���A���A��^A��hA��jA��A�`BA�  A��A�?}A�VA��;A�O�A��`A��FA�n�A���A�XA���A��hA�A��A�1'A�=qA��\A�;dA��#A� �A���A�K�A�`BA��A~jA{�Aw��Au��At��Atr�AtQ�Arz�An{Aj�RAg�Af��Af�+Ae�Ad�\Ac&�A`��A`1A_hsA^  AZ��AX��AW��AUVAS�wAR��AP1'AO"�ANbNAM�^AJ�AF�jAE��AF-AD�AC��AB1'AA��A@�A?ƨA>~�A=C�A<M�A;��A:�A8�yA7�7A6�uA5�A4bNA2n�A1��A1;dA0��A/��A,�\A+ƨA+��A+�A+�A)S�A(9XA({A'|�A&�A%�A"��A VA�RA�A  A?}A��AffA/AM�AƨA��AdZA/A�At�A��A9XA�A��A��A�;A/A{AhsA"�A	�
A	&�A�/A�9A�uA�A�yAbNA�FA�9A��A�AhsA �\A 9XA {@�~�@�Z@��H@�/@�J@���@�P@���@��/@���@�7@��@��T@�x�@��`@�@�b@�9X@�V@�j@�D@�Z@��@��@�7@�@���@�|�@�;d@�v�@���@��@�E�@أ�@��@�?}@��`@�1'@�@Ѻ^@���@�K�@�7L@�j@˶F@��@���@�Ĝ@�ƨ@ǅ@�K�@��@Ɵ�@�V@�=q@��T@��@å�@�@��@�b@�\)@��+@��@��y@��@�x�@�?}@�&�@��@���@��j@���@�5?@�9X@�
=@�^5@���@�V@�A�@�|�@��@�@��+@�@�Ĝ@���@�C�@��H@���@�ȴ@���@��`@�bN@�+@���@�~�@�-@�@��T@���@�G�@�/@�7L@�7L@�?}@�O�@�&�@�&�@�p�@�`B@�?}@�?}@�G�@�7L@�V@��j@�Q�@���@�n�@��-@��-@���@�@�p�@�G�@�%@��`@���@��@��@�I�@��;@�ƨ@�t�@��@�b@�1@��m@��
@��@�S�@��@�@��@��H@��y@��\@��\@�J@��#@���@�O�@���@���@��F@���@�l�@�33@�^5@�`B@�r�@��D@�b@�o@���@�&�@�r�@���@��
@���@��P@�
=@��R@�~�@�{@���@���@�V@�ff@�5?@��@���@��`@��D@��
@�33@�ȴ@��\@�J@���@�p�@��h@�p�@�x�@�X@�/@��`@�V@��@��j@�1'@�1@���@���@�;d@�@��H@�ȴ@��R@�^5@��@��@���@��-@���@��h@�x�@�`B@�?}@�V@���@�r�@�9X@��@��@�  @��;@���@�|�@�dZ@�S�@�;d@��@���@��R@��\@�^5@��@��-@��h@�x�@�V@��/@�Ĝ@�r�@���@��w@�dZ@�"�@�@��HG�O�@��9@v�@m%F@e�7@\h�@T7�@N@F	@?��@:��@4��@.�@(��@$�U@!Y�@�Q@�@�@�@�a@ی111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB_;B`BB_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B`BB_;B`BB`BB`BB`BB_;B`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BBgmBp�Bs�Bv�B� B� B�B�B�B� B�B�B�B�B�B�B�B�B�B� B}�B|�B{�Bz�By�B{�Bw�Bw�Bx�Bw�Bv�Bq�Bp�Bo�Bo�Bl�BiyBbNB]/B\)BYBT�BT�BP�BL�B?}B&�B�B�B�BoBDB%B  B�B�ZB�/B��BŢB�?B�B��B��B� By�Br�BiyBT�BM�BE�BC�B>wB33B,B$�B�BoB%B
��B
�B
�5B
�B
��B
��B
��B
|�B
o�B
`BB
8RB
$�B
DB	��B	�B	�B	�B	�BB	�XB	��B	�%B	{�B	v�B	r�B	l�B	bNB	VB	XB	T�B	L�B	(�B	�B	{B	+B��B��B�B�sB�NB�fB�B�jB��B�5B�#B��B��B��B�B�B��B��B��B��B��BǮB��B�jB�XB�9B�B��B��B��B��B�1B�+B�7B�DB�{B�PB�1B�1B�1B�%B�Bp�BaHBZB[#B]/BXBYBYBT�BP�BO�BJ�BH�BC�B@�B?}B>wB=qB<jB;dB<jB9XB8RB9XB6FB7LB6FB49B33B2-B1'B2-B1'B1'B1'B1'B/B.B/B-B,B+B+B+B)�B)�B+B'�B)�B'�B'�B'�B&�B'�B&�B'�B(�B)�B,B.B33B49B5?B5?B7LB9XB9XB;dB<jB<jB<jB=qB<jB>wB>wBA�BD�BC�BD�BE�BF�BH�BI�BM�BP�BQ�BS�BT�BW
BYB[#B[#B[#B\)B]/B]/B]/B]/B_;BbNBhsBiyBk�Bm�Bo�Bs�By�B{�B}�B}�B~�B~�B~�B~�B�B�+B�PB�\B�\B�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�XB�qB�wB��BBĜBɺB��B�B�
B�B�B�#B�HB�B��B	  B	B	B	B	+B	JB	PB	\B	PB	DB	JB	bB	{B	�B	�B	�B	 �B	"�B	%�B	)�B	+B	-B	/B	33B	5?B	8RB	@�B	C�B	I�B	K�B	N�B	Q�B	S�B	VB	W
B	YB	[#B	^5B	aHB	bNB	cTB	e`B	hsB	k�B	n�B	o�B	o�B	o�B	o�B	q�B	q�B	r�B	v�B	x�B	v�B	v�B	v�B	x�B	�B	�B	�B	�B	�%B	�=B	�JB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�?B	�FB	�FB	�FB	�LB	�^B	�jB	�jB	�qB	�qB	��B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�NB	�NB	�NB	�TB	�TB	�TB	�`B	�fB	�iB	�nB	��B
hB
�B
#�B
*KB
1�B
9�B
A�B
JrB
N�B
S[B
W
B
[	B
aHB
e�B
j�B
oB
q�B
u?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BV�BW�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BW�BV�BW�BW�BW�BW�BV�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�BW�B_
BhABkTBngBw�Bw�Bx�Bx�Bx�Bw�B{�Bz�Bz�Bz�Bz�B{�Bz�Bz�Bz�Bw�Bu�Bt�Bs�Br�Bq|Bs�BopBopBpvBopBnjBiLBhFBg@Bg@Bd-BaBY�BT�BS�BP�BL�BL�BH�BDrB7#B�BCB0B*B
B�B��B��B�VB�B��BȔB�RB��B��B�YB�4Bw�Bq�BjfBa0BL�BE�B=[B;OB61B*�B#�B�BnB
,B
��B
�B
�KB
��B
��B
ȧB
�FB
��B
t�B
ggB
XB
0B
�B
B	�B	�}B	�dB	�RB	�B	�/B	��B	~ B	s�B	n�B	j�B	dhB	Z+B	M�B	O�B	L�B	D�B	 �B	�B	^B�B��B��B�B�ZB�6B�MB�B�UB�nB�B�B��B��B��B��B��B��B��B��BžB¬B��B�oB�WB�EB�&B�B��B��B��B��B�"BB�(B�5B�lB�AB�#B�#B�#B~Bx�Bh�BY=BRBSBU%BPBQBQBL�BH�BG�BB�B@�B;�B8|B7vB6pB5jB4cB3]B4cB1RB0LB1RB.@B/FB.AB,4B+.B*(B)"B*(B)"B)"B)"B)"B'B&B'B%
B$B"�B"�B"�B!�B!�B"�B�B!�B�B�B�B�B�B�B�B �B!�B$B&B+1B,7B-=B-=B/JB1VB1VB3bB4hB4hB4hB5oB4hB6uB6uB9�B<�B;�B<�B=�B>�B@�BA�BE�BH�BI�BK�BL�BOBQBS BS BS BT&BU,BU,BU,BU,BW8BZKB`pBavBc�Be�Bg�Bk�Bq�Bs�Bu�Bu�Bv�Bv�Bv�Bv�B{B'B�LB�XB�XB�^B�jB�|B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�RB�kB�qB�|B��B��B��B��B��B�B�B�B�B�?B�B��B��B�B�B�B� B	?B	EB	QB	EB	9B	?B	WB	pB	�B	�B	�B	�B	�B	�B	!�B	"�B	%B	'B	+&B	-2B	0DB	8uB	;�B	A�B	C�B	F�B	I�B	K�B	M�B	N�B	QB	SB	V%B	Y8B	Z>B	[DB	]PB	`cB	cuB	f�B	g�B	g�B	g�B	g�B	i�B	i�B	j�B	n�B	p�B	n�B	n�B	n�B	p�B	x�B	{B	y�B	|B	~B	�+B	�8B	�DB	�JB	�]B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�2B	�2B	�2B	�8B	�JB	�VB	�VB	�]B	�]B	�oB	�zB	��B	��B	��B	��B	��B	��B	¬B	ĸB	ĸB	žB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�,B	�8B	�8B	�8B	�>B	�>B	�>B	�JG�O�B	�RB	�WB	��B
	PB
�B
�B
"2B
)�B
1�B
9�B
BXB
F�B
KAB
N�B
R�B
Y.B
]�B
b�B
g B
i�B
m$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144152022020411441520220204114415  AO  ARCAADJP                                                                    20200619170904    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170904  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170904  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114415  IP                  G�O�G�O�G�O�                