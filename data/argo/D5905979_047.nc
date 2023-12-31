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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170904  20220204114415  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               /A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؘ�T�]1   @ؘ����@6��S����c�x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    /A   B   B   @�  @�33A   A   A@  A`  A�  A�  A�  A�33A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�qD�*�D�aHD���D�� D��D�_�D���D���D�RD�c3D���D��=D��D�V�Dڀ�D�޸D�${D�P�D�D�ؤ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@�
=@��
A�A=�A]�A}�A���A���A�(�A�(�A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��>B��qB��qB��qB��>B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D �HDw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Dy�D�&fD�]D��qD���D��D�[�D��qD���D�)D�_
D���D��D�qD�R�D�|�D�ڏD� RD�L�D�qD��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AÛ�AÝ�Aá�Aç�Aé�AìAîAîAð!Að!Að!Aò-Að!AìAìAîAð!Aô9AöFAøRAú^AøRAüjA���A���A�A�A�A�A�A�ĜA�ĜA�ĜA�ĜA���A���A���A���A���Aá�AÉ7AÁA�p�A�=qA¡�A��A�5?A�K�A���A�`BA��A�7LA���A��A���A�"�A��jA�M�A�oA��hA�7LA�=qA�VA�9XA�bNA��`A��wA�O�A��yA�;dA�A��uA�?}A��uA�ZA��\A��RA�|�A�oA��/A��+A���A�l�A�;dA���A�A�A�Q�A��TA��!A�jA�=qA�A�VA�-A��DA�(�A��/A���A���A���A�A��A��\A�
=A�JA��A��-A�9XA��^A���A�1A�dZA�5?A��A�$�A|VAy�7Aw�7ArE�An�`Al�Ak��Ak�AjE�Ag7LAd��Ad1Ac�Acl�Aa�;A_�-A]��A]oA\�AZffAX1'AU�PAT9XARbAP�DAO��AO��AO�PAO|�AN��AL�!AJ�/AIAG��AF�HAF$�AES�ADA�AC�TAC��ABr�AA��AAVA?��A<�DA;�^A;dZA:��A9�A7��A77LA6��A5��A5�A4Q�A2ffA0�HA0�A/�A-S�A+�;A*�HA)x�A(bA&�`A%�TA%�A%?}A#�7A!A �uAx�AI�A�hA�A-A�A�A��A�uAI�Ap�A�9A�Az�A��A��Ap�AG�A�AhsA�A�A��A
�uA	C�AĜA{A/AȴAE�A�;AVAl�A=qA�FA`BA&�A =q@�"�@�7L@�+@���@�
=@�=q@�&�@�  @��/@�7@�dZ@�&�@�1@�ff@�@��@�-@�V@�j@��;@�"�@�
=@�@��@���@���@�hs@�K�@�Z@�|�@�ȴ@ׅ@֧�@�@�7L@���@�z�@�9X@��;@�@θR@��;@�\)@�
=@��H@ʟ�@�7L@�
=@�V@�(�@Ý�@�o@¸R@°!@�n�@���@��D@���@���@��P@�|�@�+@���@��T@�V@�9X@�ƨ@���@��@��@�&�@��@�Ĝ@��;@��@���@�  @�o@���@�p�@��j@�  @�ƨ@���@�l�@�ȴ@�@��h@�x�@�hs@�V@�j@��@���@���@�$�@���@��j@��F@�\)@�\)@�|�@��P@���@�\)@��y@��+@�{@�X@�?}@�/@�&�@�G�@�O�@��@�%@�%@�%@�%@���@��@��`@��`@��/@���@�Q�@�o@��T@�7L@��@�V@���@�j@�Q�@��u@��D@��9@�Z@�|�@�;d@�;d@�dZ@�|�@��w@�ƨ@�dZ@��@���@��H@�v�@�-@�@���@�{@�J@�V@�5?@��@�@�?}@���@���@�@���@��h@��/@��j@��u@��
@�ff@�J@�X@���@��@��P@���@�5?@��#@�`B@��@�Z@�I�@�1@���@���@��@��R@�n�@�-@�{@�@��T@��^@��7@�x�@�&�@��/@�Ĝ@���@��@�Z@�A�@�b@��;@���@�\)@��@��+@�ff@�-@�@�hs@��@�Z@�1'@�1@���@�33@��y@�ȴ@���@���@���@��@���@�ff@�M�@�5?@�$�@�{@�{@��@���@�X@���@��`@�Ĝ@��j@��9@���@��@�;d@��!@�5?@�{@��T@�@���@��j@�  @��@�  @��@��@��@��
@���@�C�@���@��^@�`B@�x�@�`B@�?}@�/@�!�@v0U@k�q@fh
@\7@W@Sj�@L�?@D�@;�@6�B@2�\@-��@*Ov@$C-@�@Q@��@�@&@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AÛ�AÝ�Aá�Aç�Aé�AìAîAîAð!Að!Að!Aò-Að!AìAìAîAð!Aô9AöFAøRAú^AøRAüjA���A���A�A�A�A�A�A�ĜA�ĜA�ĜA�ĜA���A���A���A���A���Aá�AÉ7AÁA�p�A�=qA¡�A��A�5?A�K�A���A�`BA��A�7LA���A��A���A�"�A��jA�M�A�oA��hA�7LA�=qA�VA�9XA�bNA��`A��wA�O�A��yA�;dA�A��uA�?}A��uA�ZA��\A��RA�|�A�oA��/A��+A���A�l�A�;dA���A�A�A�Q�A��TA��!A�jA�=qA�A�VA�-A��DA�(�A��/A���A���A���A�A��A��\A�
=A�JA��A��-A�9XA��^A���A�1A�dZA�5?A��A�$�A|VAy�7Aw�7ArE�An�`Al�Ak��Ak�AjE�Ag7LAd��Ad1Ac�Acl�Aa�;A_�-A]��A]oA\�AZffAX1'AU�PAT9XARbAP�DAO��AO��AO�PAO|�AN��AL�!AJ�/AIAG��AF�HAF$�AES�ADA�AC�TAC��ABr�AA��AAVA?��A<�DA;�^A;dZA:��A9�A7��A77LA6��A5��A5�A4Q�A2ffA0�HA0�A/�A-S�A+�;A*�HA)x�A(bA&�`A%�TA%�A%?}A#�7A!A �uAx�AI�A�hA�A-A�A�A��A�uAI�Ap�A�9A�Az�A��A��Ap�AG�A�AhsA�A�A��A
�uA	C�AĜA{A/AȴAE�A�;AVAl�A=qA�FA`BA&�A =q@�"�@�7L@�+@���@�
=@�=q@�&�@�  @��/@�7@�dZ@�&�@�1@�ff@�@��@�-@�V@�j@��;@�"�@�
=@�@��@���@���@�hs@�K�@�Z@�|�@�ȴ@ׅ@֧�@�@�7L@���@�z�@�9X@��;@�@θR@��;@�\)@�
=@��H@ʟ�@�7L@�
=@�V@�(�@Ý�@�o@¸R@°!@�n�@���@��D@���@���@��P@�|�@�+@���@��T@�V@�9X@�ƨ@���@��@��@�&�@��@�Ĝ@��;@��@���@�  @�o@���@�p�@��j@�  @�ƨ@���@�l�@�ȴ@�@��h@�x�@�hs@�V@�j@��@���@���@�$�@���@��j@��F@�\)@�\)@�|�@��P@���@�\)@��y@��+@�{@�X@�?}@�/@�&�@�G�@�O�@��@�%@�%@�%@�%@���@��@��`@��`@��/@���@�Q�@�o@��T@�7L@��@�V@���@�j@�Q�@��u@��D@��9@�Z@�|�@�;d@�;d@�dZ@�|�@��w@�ƨ@�dZ@��@���@��H@�v�@�-@�@���@�{@�J@�V@�5?@��@�@�?}@���@���@�@���@��h@��/@��j@��u@��
@�ff@�J@�X@���@��@��P@���@�5?@��#@�`B@��@�Z@�I�@�1@���@���@��@��R@�n�@�-@�{@�@��T@��^@��7@�x�@�&�@��/@�Ĝ@���@��@�Z@�A�@�b@��;@���@�\)@��@��+@�ff@�-@�@�hs@��@�Z@�1'@�1@���@�33@��y@�ȴ@���@���@���@��@���@�ff@�M�@�5?@�$�@�{@�{@��@���@�X@���@��`@�Ĝ@��j@��9@���@��@�;d@��!@�5?@�{@��T@�@���@��j@�  @��@�  @��@��@��@��
@���@�C�@���@��^@�`B@�x�@�`B@�?}G�O�@�!�@v0U@k�q@fh
@\7@W@Sj�@L�?@D�@;�@6�B@2�\@-��@*Ov@$C-@�@Q@��@�@&@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBe`Be`Be`Be`BffBe`BffBe`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`BffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBffBgmBl�Bk�Bk�BjBm�Bo�Bo�Br�B{�B�B�DB�%B�B�B�B�7B�B�B�+B�B�B�B�B� B~�Bz�By�By�Bx�Bx�Bw�Bv�Bu�Bq�Bo�Bl�BdZBZBXBVBO�B@�B2-B�BJBB��B�B�mB�`B�BB�5B��BȴB�wB�9B�B��B��B�DB|�Bl�BVBM�BE�B9XB+B	7B
�B
�NB
��B
�9B
��B
�B
x�B
ffB
8RB
�B
VB	�B	��B	B	�?B	�'B	�B	��B	�B	|�B	z�B	x�B	w�B	n�B	dZB	dZB	aHB	\)B	P�B	>wB	6FB	0!B	$�B	�B	�B	�B	�B	�B	JB��B��B�B�TB�B�
B��BɺBǮB��B�XB�9B�B��B��B��B��B��B�DB�7B�1B�B~�B|�By�Bs�Bs�Br�Bq�BiyBiyBe`BdZBe`Be`BgmBjBjBffBcTB_;B_;B]/B_;B^5B]/B[#B[#BZBYBXBVBVBS�BQ�BN�BL�BK�BJ�BI�BF�BF�BC�BA�B?}B?}B>wB>wB=qB=qB;dB;dB;dB8RB6FB6FB5?B6FB7LB7LB7LB8RB7LB6FB5?B5?B8RB8RB8RB8RB9XB9XB9XB8RB8RB:^B8RB:^B:^B:^B9XB9XB9XB9XB8RB<jB<jB<jB=qBB�BC�BD�BE�BE�BE�BF�BE�BE�BP�BR�BR�BS�BS�BR�BVB\)BaHBcTBdZBffBgmBffBgmBjBm�Bo�Bp�Bo�Bo�Bq�Bq�Bt�Bv�By�Bz�B{�B}�B�B�B�B�%B�7B�PB�\B�VB�\B�PB�PB�VB�bB�hB�hB�hB��B��B��B��B��B��B��B��B��B��B�B�B�LB�jB�}B��BĜBƨBǮBɺB��B��B��B��B��B�B�B�BB�`B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	B	B	
=B	VB	hB	�B	�B	 �B	(�B	.B	+B	+B	,B	.B	/B	33B	6FB	:^B	:^B	:^B	:^B	:^B	>wB	B�B	F�B	L�B	M�B	O�B	P�B	R�B	ZB	\)B	bNB	gmB	iyB	jB	l�B	r�B	r�B	s�B	t�B	r�B	q�B	p�B	p�B	q�B	q�B	r�B	q�B	s�B	t�B	u�B	y�B	y�B	z�B	z�B	{�B	� B	�B	�B	�7B	�=B	�=B	�JB	�VB	�\B	�\B	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�?B	�FB	�FB	�FB	�RB	�qB	�wB	�}B	��B	��B	��B	B	B	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�HB	�`B	�mB	��B
aB
�B
�B
)B
 �B
(>B
0;B
:�B
B�B
H�B
M�B
VB
Z�B
_�B
dZB
i*B
kB
m�B
sB
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B]	B]	B]	B]	B^B]	B^B]	B]	B]	B]	B]	B]	B]	B]	B]	B]	B]	B]	B]	B]	B]	B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B_Bd3Bc.Bc.Bb(Be9BgFBgGBjXBs�B|�B��B}�Bz�By�By�B��Bz�Bz�B~�B{�B{�By�By�Bw�Bv�Br�Bq�Bq�Bp�Bp�Bo{BnuBmoBiWBgKBd8B\BQ�BO�BM�BG�B84B)�BZB�B��B�B�[B�%B�B��B��BɦB�nB�2B��B��B��B�^B�Bt�BdLBM�BE�B=fB1B"�B B
�\B
�B
��B
�B
��B
{�B
p�B
^;B
0*B
�B
1B	�|B	ĭB	�pB	�!B	�	B	��B	�kB	{�B	t�B	r�B	p�B	o�B	f�B	\CB	\CB	Y1B	TB	H�B	6cB	.2B	(B	�B	�B	�B	�B	�B	�B	:B��B�B�B�GB�B��B��B��B��B�yB�NB�0B�B��B��B��B��B�zB�>B�1B�,B|Bv�Bt�Bq�Bk�Bk�Bj�Bi�BawBawB]^B\XB]_B]_B_lBb}Bb~B^eB[SBW;BW;BU/BW;BV5BU/BS$BS$BRBQBPBNBNBK�BI�BF�BD�BC�BB�BA�B>�B>�B;�B9�B7�B7�B6|B6|B5vB5vB3iB3iB3jB0XB.LB.LB-EB.LB/RB/RB/SB0YB/SB.MB-FB-FB0YB0YB0ZB0ZB1`B1`B1`B0ZB0ZB2fB0ZB2fB2fB2fB1`B1`B1`B1`B0ZB4rB4rB4rB5yB:�B;�B<�B=�B=�B=�B>�B=�B=�BH�BJ�BJ�BL BL BJ�BNBT1BYPB[\B\bB^mB_tB^mB_tBb�Be�Bg�Bh�Bg�Bg�Bi�Bi�Bl�Bn�Bq�Br�Bs�Bu�B|B}%B}%B~+B�=B�VB�bB�\B�bB�VB�VB�\B�hB�nB�nB�nB��B��B��B��B��B��B��B��B��B��B�B� B�PB�nB��B��B��B��B��B��B��B��B��B��B��B�B�B�DB�bB�B�B�B�B�B�B��B��B��B��B�B�B�B�B�B�B	=B	VB		gB	�B	�B	�B	 �B	&B	# B	# B	$B	&B	'B	+0B	.CB	2[B	2[B	2[B	2[B	2[B	6tB	:�B	>�B	D�B	E�B	G�B	H�B	J�B	RB	T$B	ZIB	_gB	asB	byB	d�B	j�B	j�B	k�B	l�B	j�B	i�B	h�B	h�B	i�B	i�B	j�B	i�B	k�B	l�B	m�B	q�B	q�B	r�B	r�B	s�B	w�B	zB	}B	�0B	�6B	�6B	�CB	�OB	�UB	�UB	�hB	�sB	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�6B	�=B	�=B	�=B	�IB	�gB	�mB	�sB	�yB	�yB	�yB	��B	��B	��B	��B	·B	ýB	ýB	ýB	ýB	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�=B	�UG�O�B	�~B	�TB	��B

�B
B
�B
 0B
(-B
2�B
:�B
@qB
E�B
NB
R�B
W�B
\JB
aB
b�B
e�B
kB
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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