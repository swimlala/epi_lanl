CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:24Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170924  20220204114424  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @� ���^p1   @� �5��@71&�x���b�r� Ĝ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�(RD�O\D���D�ӅD�$)D�X�D���D�� D��D�R�D���DǝD�!�D�N�Dګ�D��HD�)D�\{D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@��
@��
A�A=�A]�A}�A���A���A���A���A�(�A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?{BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D~D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6�D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt�Dy��D�$)D�K3D��{D��\D�  D�T{D��qD���D�{D�NfD���Dǘ�D��D�J�Dڧ\D��D� D�XRD�fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�XA�VA�ffA�t�A�r�A�t�A�t�A�z�A�|�A�|�A�|�A�~�A�~�A��A�~�A��A��A��A��A�|�A�x�A�t�A�v�A�x�A��A��A��A��A��A��A��A��A��A��A��A��A�r�A�jA�^5A�C�A��;A�ffA�"�A�%A��A��RA�r�A�M�A���A���A���A�I�A��A�O�A���A��jA�jA���A�33A��\A��FA��A��
A�?}A��9A�O�A���A���A�$�A�t�A�p�A�jA�"�A�&�A���A�33A��FA�VA��/A�S�A�7LA�oA���A��!A�%A���A�I�A���A��A���A�7LA���A�33A��;A���A�Q�A��jA�=qA���A�;dA���A�ȴA�dZA���A�VA�C�A�JA��A�x�A�VA�;dA�x�A�(�A��A���A���A�
=A���A���A�\)A��A���A��A�dZA�A�O�A��#A�(�A�K�A|��Ax��AvbAs�Arn�Ap�yAm��Ai`BAg�mAe�FAb��Aa�hA_��A]�7A[�AXAT$�AQ�AN��AMVAK�AJ��AJ5?AI��AH�/AG�AF�AFbNADQ�AB�A@(�A?�A>�!A=ƨA:�DA7��A8�A7�hA6��A5l�A3��A17LA/A-p�A,Q�A+XA*ĜA*�A(��A&��A%x�A#��A"Q�A Q�A�A�uA��A�DAC�An�A��A�jA�#A�7AAjAO�A(�A��A�;A33Ar�A�A�#AG�A�9AM�AO�A�RA��A�A
n�A
Q�A
bA	t�A	��A	x�A	%AM�AA�A�A�wA9XAA�A��A�A/A �@�+@��y@�ff@�%@�\)@�-@��@�|�@�`B@�V@�w@��@��D@�ȴ@�V@�A�@�ff@�@��@�P@�o@�$�@�@�?}@��@�ȴ@١�@��@؋D@ו�@�M�@թ�@�`B@ԛ�@�(�@ӶF@���@Ѓ@�t�@Χ�@Ώ\@͉7@�Z@�5?@�@ȃ@��@��@�$�@�J@���@őh@���@öF@�^5@��@��F@�o@��@��@��!@���@�p�@��`@��`@��`@���@��+@��9@�A�@�\)@�=q@�$�@�@��/@�j@���@�~�@�~�@�5?@�$�@�$�@�J@���@�Ĝ@���@�33@�{@��@��j@�z�@���@�+@�v�@���@�x�@�%@��@��P@���@��P@���@�Z@��/@�+@��!@��@�@��@��7@�9X@�C�@��@��!@�ff@�=q@���@�z�@�ƨ@�|�@�o@�
=@�
=@���@�M�@�ff@���@�1@��@�$�@���@�?}@��/@��@��@�b@��P@�dZ@��@�n�@�{@��T@��@���@��-@�`B@�7L@�7L@�hs@���@�p�@�`B@���@�bN@��w@�"�@��\@�$�@��T@���@��@�`B@��@���@��@���@�r�@�j@�9X@��m@���@��F@���@���@�|�@�\)@�dZ@�t�@��P@���@�|�@�o@��@��!@��R@�ȴ@�ȴ@��R@���@�$�@��^@�X@��/@��9@��F@�v�@�$�@���@���@���@���@�E�@���@�x�@��@��`@�V@�j@� �@��m@��F@�t�@�+@�@���@��!@���@�v�@�ff@�ff@�~�@�ff@�@�?}@���@�O�@���@�Z@�1'@�1'@�I�@�r�@��@��@�`B@�p�@�O�@��@��
@��F@�33@���@��\@�5?@��@���@�@���@���@�@��@���@�ȴ@��H@���@���@���@���@�v�@�J�@�A@u��@n^5@e�9@^z@W��@Pe�@G�@?s@9V@3��@/>�@)��@$?�@O@��@(@~�@e�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�XA�XA�VA�ffA�t�A�r�A�t�A�t�A�z�A�|�A�|�A�|�A�~�A�~�A��A�~�A��A��A��A��A�|�A�x�A�t�A�v�A�x�A��A��A��A��A��A��A��A��A��A��A��A��A�r�A�jA�^5A�C�A��;A�ffA�"�A�%A��A��RA�r�A�M�A���A���A���A�I�A��A�O�A���A��jA�jA���A�33A��\A��FA��A��
A�?}A��9A�O�A���A���A�$�A�t�A�p�A�jA�"�A�&�A���A�33A��FA�VA��/A�S�A�7LA�oA���A��!A�%A���A�I�A���A��A���A�7LA���A�33A��;A���A�Q�A��jA�=qA���A�;dA���A�ȴA�dZA���A�VA�C�A�JA��A�x�A�VA�;dA�x�A�(�A��A���A���A�
=A���A���A�\)A��A���A��A�dZA�A�O�A��#A�(�A�K�A|��Ax��AvbAs�Arn�Ap�yAm��Ai`BAg�mAe�FAb��Aa�hA_��A]�7A[�AXAT$�AQ�AN��AMVAK�AJ��AJ5?AI��AH�/AG�AF�AFbNADQ�AB�A@(�A?�A>�!A=ƨA:�DA7��A8�A7�hA6��A5l�A3��A17LA/A-p�A,Q�A+XA*ĜA*�A(��A&��A%x�A#��A"Q�A Q�A�A�uA��A�DAC�An�A��A�jA�#A�7AAjAO�A(�A��A�;A33Ar�A�A�#AG�A�9AM�AO�A�RA��A�A
n�A
Q�A
bA	t�A	��A	x�A	%AM�AA�A�A�wA9XAA�A��A�A/A �@�+@��y@�ff@�%@�\)@�-@��@�|�@�`B@�V@�w@��@��D@�ȴ@�V@�A�@�ff@�@��@�P@�o@�$�@�@�?}@��@�ȴ@١�@��@؋D@ו�@�M�@թ�@�`B@ԛ�@�(�@ӶF@���@Ѓ@�t�@Χ�@Ώ\@͉7@�Z@�5?@�@ȃ@��@��@�$�@�J@���@őh@���@öF@�^5@��@��F@�o@��@��@��!@���@�p�@��`@��`@��`@���@��+@��9@�A�@�\)@�=q@�$�@�@��/@�j@���@�~�@�~�@�5?@�$�@�$�@�J@���@�Ĝ@���@�33@�{@��@��j@�z�@���@�+@�v�@���@�x�@�%@��@��P@���@��P@���@�Z@��/@�+@��!@��@�@��@��7@�9X@�C�@��@��!@�ff@�=q@���@�z�@�ƨ@�|�@�o@�
=@�
=@���@�M�@�ff@���@�1@��@�$�@���@�?}@��/@��@��@�b@��P@�dZ@��@�n�@�{@��T@��@���@��-@�`B@�7L@�7L@�hs@���@�p�@�`B@���@�bN@��w@�"�@��\@�$�@��T@���@��@�`B@��@���@��@���@�r�@�j@�9X@��m@���@��F@���@���@�|�@�\)@�dZ@�t�@��P@���@�|�@�o@��@��!@��R@�ȴ@�ȴ@��R@���@�$�@��^@�X@��/@��9@��F@�v�@�$�@���@���@���@���@�E�@���@�x�@��@��`@�V@�j@� �@��m@��F@�t�@�+@�@���@��!@���@�v�@�ff@�ff@�~�@�ff@�@�?}@���@�O�@���@�Z@�1'@�1'@�I�@�r�@��@��@�`B@�p�@�O�@��@��
@��F@�33@���@��\@�5?@��@���@�@���@���@�@��@���@�ȴ@��H@���@���@���@���G�O�@�J�@�A@u��@n^5@e�9@^z@W��@Pe�@G�@?s@9V@3��@/>�@)��@$?�@O@��@(@~�@e�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�yB
�yB
�B
�TB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�B
�yB
�yB
�yB
�B
�B
�B
��B
��B�B)�B2-B8RB>wBA�BK�BR�BffBp�Bw�B}�B�%B�=B�\B��B��B�'B�qB��B�`B�B��B�B�B-B33B=qBF�BH�BR�BXB[#B]/B_;B\)B\)BZBZBZBS�BQ�BQ�BO�BL�BG�BE�BB�BB�B>wB8RB49B/B,B �B�BbBDB��B�B�B�`B�;B�B��B�qB��B�uB�1B�Br�Be`BC�B)�B�BB
��B
�B
�sB
�5B
�
B
ƨB
�XB
�B
��B
��B
�PB
z�B
^5B
H�B
%�B
DB	��B	�B	�BB	ɺB	�B	��B	�{B	~�B	s�B	hsB	W
B	K�B	49B	�B	1B��B�B�fB�;B�/B�)B�B��B��B��BǮB�jB�FB�'B�B��B��B��B��B��B��B��B��B�=B|�Bv�Bu�Bq�Bn�Bl�BjBe`BdZBe`BaHB^5B\)BZBYBW
BT�BVBVBVBVBVBT�BS�BR�BT�BS�BW
BYB]/B\)B\)B^5B^5BaHB`BBaHB_;B\)BYBZB\)B\)BgmBffBffBdZB_;BXBVBXBdZBiyBcTBbNB`BB_;B]/BW
BW
BVBS�BVBS�BR�BT�BS�BS�BVBVBS�BS�BP�BM�BK�BE�BB�BC�BC�BC�BE�BI�BI�BF�BE�BF�BG�BH�BH�BK�BL�BM�BL�BL�BL�BL�BL�BQ�B^5BaHBaHB\)B]/B_;B`BBe`BiyBiyBk�Bq�Bs�Bs�Bv�Bx�Bx�By�B{�B~�B�B�B�+B�\B�hB�{B��B�{B�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�FB�LB�LB�RB�FB�?B�LB�dB��BĜBĜBÖBĜBĜBĜBƨB��B��B��B�)B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	%B	+B		7B	\B	uB	�B	)�B	49B	9XB	6FB	7LB	7LB	8RB	;dB	>wB	?}B	?}B	@�B	C�B	E�B	E�B	G�B	I�B	J�B	J�B	K�B	N�B	R�B	W
B	[#B	ZB	]/B	bNB	dZB	dZB	ffB	jB	m�B	p�B	r�B	s�B	s�B	v�B	y�B	{�B	|�B	~�B	�B	�B	�B	�+B	�1B	�=B	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�?B	�?B	�?B	�LB	�RB	�XB	�^B	�qB	�wB	�}B	�}B	��B	ÖB	ĜB	ƨB	ƨB	ǮB	ɺB	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	�)B	�5B	�/B	�/B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�5B	�fB	�sB	�sB	�B	��B	��B	��B	��B	��B	��B	��B
9B
�B
�B
�B
*�B
.�B
5tB
7�B
A�B
G�B
OB
R�B
X_B
^B
d�B
kB
n�B
sB
vzB
x�111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
ܗB
ܗB
ݝG�O�B
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ܗB
ݝB
ܗB
ܗB
ܗB
ޣB
��B
��B
��B
�B�BB%DB+hB1�B4�B>�BFBYxBc�Bj�BqBy4B}LB�kB��B��B�3B�|B��B�hB�B��B	�B�B B&5B0rB9�B;�BE�BKBN#BP/BR;BO)BO)BMBMBMBF�BD�BD�BB�B?�B:�B8�B5�B5�B1|B+XB'@B""BB�B�BmB�PB��B�BݎB�pB�LB�!B��B��B�B��B{KBt!Be�BX~B6�B!B�B
�IB
��B
��B
۠B
�cB
�9B
��B
��B
�HB
�B
��B
��B
nB
QqB
;�B
&B	��B	�#B	��B	ӍB	�B	�_B	�B	��B	rQB	gB	[�B	JeB	?$B	'�B	B��B�MB��B��BҥBЙBϓB�{B�cB�KB�?B�B��B��B��B��B�mB�OB�B�hB�bB�\B�PB�&B}�BpfBjBBi<Be$BbB`B]�BX�BW�BX�BT�BQ�BO�BM�BL�BJ�BH}BI�BI�BI�BI�BI�BH~BGxBFsBHBGyBJ�BL�BP�BO�BO�BQ�BQ�BT�BS�BT�BR�BO�BL�BM�BO�BO�BZ�BY�BY�BW�BR�BK�BI�BK�BW�B\�BV�BU�BS�BR�BP�BJ�BJ�BI�BG}BI�BG}BFwBH�BG}BG}BI�BI�BG~BG~BDkBAZB?NB9*B6B7B7B7B9+B=BB=CB:1B9+B:1B;7B<=B<>B?PB@VBA\B@VB@VB@VB@WB@WBEuBQ�BT�BT�BO�BP�BR�BS�BX�B]B]B_Be1Bg=Bg=BjPBl\Bl\BmbBonBr�Bu�Bx�Bz�B��B��B� B�B� B��B�B�B�%B�+B�+B�+B�B�%B�7B�VB�VB�aB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�*B�CB�UB�fBϩB��B��B��B�B�.B�:B�dB�YB�SB�YB�YB�YB�YB�eB�xB�B��B��B��B��B	�B	�B	B	vB	'�B	,�B	)�B	*�B	*�B	+�B	.�B	1�B	2�B	2�B	3�B	7B	9B	9B	;%B	=1B	>8B	>8B	?>B	BPB	FhB	J�B	N�B	M�B	P�B	U�B	W�B	W�B	Y�B	]�B	aB	dB	f$B	g*B	g*B	j<B	mNB	oZB	paB	rmB	u~B	v�B	x�B	z�B	{�B	}�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�.B	�5B	�:B	�@B	�FB	�LB	�_B	�eB	�eB	�kB	�qB	�qB	�kB	�eB	�eB	�kB	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�'B	�EB	�'B	�'B	�'B	�3B	�QB	�XB	�dB	ϔB	ѠB	КB	КB	̂B	̂B	͈B	�{B	̂B	͈B	͈B	͈B	ΎB	ΎB	ѠB	��B	��B	��B	�B	�%B	�%B	�2B	�2B	�7G�O�B	�VB	��B	��B
5B
TB
�B
"2B
(�B
+OB
5WB
;-B
BsB
F!B
K�B
QB
XB
^B
a�B
f|B
i�B
l8111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.13 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.012(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144242022020411442420220204114424  AO  ARCAADJP                                                                    20200619170924    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170924  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170924  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114424  IP                  G�O�G�O�G�O�                