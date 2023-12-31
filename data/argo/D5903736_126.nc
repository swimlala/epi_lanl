CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-04T02:15:21Z AOML 3.0 creation; 2016-05-31T19:14:45Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151004021521  20160531121445  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ~A   AO  4051_7090_126                   2C  D   APEX                            5368                            041511                          846 @�t0i�N�1   @�t1���@3�x����dpbM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ~A   A   A   @�ff@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  BxffB��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�	�D�<�D�p D���D��D�L�D�p D��3D�	�D�@ D��fD���D� D�6fD�y�D���D��D�I�D�|�D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  BxffB��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�	�D�<�D�p D���D��D�L�D�p D��3D�	�D�@ D��fD���D� D�6fD�y�D���D��D�I�D�|�D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A�A�1A�VA�JA�
=A�JA�VA�oA�VA�{A�JA�oA�oA�oA�{A��A�bA�oA�JA�JA�1A���A��yA㗍A�33A�1A��A�|�Aպ^A��A� �A�-Aҩ�A�jAѣ�A�1A�oA�A�(�A��A��
A˼jA�  A�ZAɓuA�ZA�bAąA×�A�33A��`A���A�n�A��FA��A�VA�+A��9A���A�^5A��A�n�A�&�A�A�p�A�`BA���A���A��A��A���A��RA�\)A��TA�t�A��A��A�"�A���A�bA���A�
=A��mA��`A��hA�(�A�n�A�v�A�oA�A�hsA� �A��TA���A��A���A�bA���A�9XA���A�33A��\A��-A��/A�5?A�Q�A��`A���A��A�O�A���A��A�33A��A�33A�5?A���A���A���A��`A�I�A���A��DA���A��7A�A��
AC�A|��A{�^AyVAwS�Au�Ar�9Ap��An��Al�`Ai�TAf�\Ac��A`�+A^��A\ȴA[|�AZ��AY;dAW�
AVv�AT��AR�\AQ��AP�AP$�AO�#AN��ALĜAJĜAH�AGAE�mAD�\A@�\A<r�A9��A7��A5�^A2�HA1��A0�`A/��A/
=A.v�A-S�A+�wA*��A*-A)�A)"�A'�A%\)A$��A$�+A$1A"��A ��AXA?}A/A��A9XA�7A�AE�A��A?}AO�Av�A{AVA�A��A�A�
AXA=qA�
A�^A;dA��A�DA�7A
=qAZAĜA��A��A�
@�ƨ@���@�I�@�{@��@��@��@�$�@���@�bN@�\)@���@�^5@�u@�dZ@��@��m@��y@�x�@�@���@��@��#@��`@�!@���@�@�z�@�dZ@�V@��@ܼj@��m@�K�@���@��@ٙ�@�Z@���@և+@�V@�G�@���@ӝ�@�K�@�"�@���@Гu@���@��@���@̼j@�j@˥�@�33@�{@���@� �@�o@Ə\@�J@���@Ĭ@�1@�@�@�{@��7@�A�@�ff@�hs@��@�(�@��w@�t�@�+@���@���@���@�Q�@��w@���@�ff@��-@�X@���@��;@�\)@�@��y@��R@��\@��+@�ff@�5?@��@���@�Ĝ@��u@�bN@�ƨ@�t�@��@���@�V@�E�@�$�@���@���@��j@�Q�@�b@��@���@�\)@��P@�bN@� �@�K�@��!@�=q@��@�O�@�%@��/@��D@��9@�j@��;@�t�@�K�@�
=@�ȴ@���@�5?@���@�&�@���@��@��@��
@�S�@�@��!@�v�@�x�@���@�(�@��w@���@�@��R@��H@���@��H@��@���@�M�@�=q@�J@��7@�`B@�G�@�7L@�&�@���@�z�@�Z@�Q�@�A�@���@��F@���@�S�@�K�@�K�@�S�@�S�@�K�@�@��R@��+@�^5@�-@���@���@���@�O�@���@��u@�z�@�b@��;@�l�@��@�@���@���@��@��@��@���@��#@��@��^@���@��@���@���@�b@�t�@�"�@��y@�~�@��@��^@�`B@�&�@��u@�I�@�ƨ@��F@��P@�"�@�ȴ@��!@���@��+@�v�@�$�@�@�@���@�@���@���@��7@�X@�/@�V@���@��9@� �@��;@��w@��P@�t�@�S�@�K�@�K�@��@���@�^5@�=q@�x�@���@���@�bN@�I�@�z�@��@�j@�I�@�(�@��@��
@��w@���@�l�@�S�@�;d@�Q�@~�@u�@n{@e@^�R@U�T@PA�@F@A��@:�!@2�@+t�@&V@!��@@%@�
@�@@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�  A�A�1A�VA�JA�
=A�JA�VA�oA�VA�{A�JA�oA�oA�oA�{A��A�bA�oA�JA�JA�1A���A��yA㗍A�33A�1A��A�|�Aպ^A��A� �A�-Aҩ�A�jAѣ�A�1A�oA�A�(�A��A��
A˼jA�  A�ZAɓuA�ZA�bAąA×�A�33A��`A���A�n�A��FA��A�VA�+A��9A���A�^5A��A�n�A�&�A�A�p�A�`BA���A���A��A��A���A��RA�\)A��TA�t�A��A��A�"�A���A�bA���A�
=A��mA��`A��hA�(�A�n�A�v�A�oA�A�hsA� �A��TA���A��A���A�bA���A�9XA���A�33A��\A��-A��/A�5?A�Q�A��`A���A��A�O�A���A��A�33A��A�33A�5?A���A���A���A��`A�I�A���A��DA���A��7A�A��
AC�A|��A{�^AyVAwS�Au�Ar�9Ap��An��Al�`Ai�TAf�\Ac��A`�+A^��A\ȴA[|�AZ��AY;dAW�
AVv�AT��AR�\AQ��AP�AP$�AO�#AN��ALĜAJĜAH�AGAE�mAD�\A@�\A<r�A9��A7��A5�^A2�HA1��A0�`A/��A/
=A.v�A-S�A+�wA*��A*-A)�A)"�A'�A%\)A$��A$�+A$1A"��A ��AXA?}A/A��A9XA�7A�AE�A��A?}AO�Av�A{AVA�A��A�A�
AXA=qA�
A�^A;dA��A�DA�7A
=qAZAĜA��A��A�
@�ƨ@���@�I�@�{@��@��@��@�$�@���@�bN@�\)@���@�^5@�u@�dZ@��@��m@��y@�x�@�@���@��@��#@��`@�!@���@�@�z�@�dZ@�V@��@ܼj@��m@�K�@���@��@ٙ�@�Z@���@և+@�V@�G�@���@ӝ�@�K�@�"�@���@Гu@���@��@���@̼j@�j@˥�@�33@�{@���@� �@�o@Ə\@�J@���@Ĭ@�1@�@�@�{@��7@�A�@�ff@�hs@��@�(�@��w@�t�@�+@���@���@���@�Q�@��w@���@�ff@��-@�X@���@��;@�\)@�@��y@��R@��\@��+@�ff@�5?@��@���@�Ĝ@��u@�bN@�ƨ@�t�@��@���@�V@�E�@�$�@���@���@��j@�Q�@�b@��@���@�\)@��P@�bN@� �@�K�@��!@�=q@��@�O�@�%@��/@��D@��9@�j@��;@�t�@�K�@�
=@�ȴ@���@�5?@���@�&�@���@��@��@��
@�S�@�@��!@�v�@�x�@���@�(�@��w@���@�@��R@��H@���@��H@��@���@�M�@�=q@�J@��7@�`B@�G�@�7L@�&�@���@�z�@�Z@�Q�@�A�@���@��F@���@�S�@�K�@�K�@�S�@�S�@�K�@�@��R@��+@�^5@�-@���@���@���@�O�@���@��u@�z�@�b@��;@�l�@��@�@���@���@��@��@��@���@��#@��@��^@���@��@���@���@�b@�t�@�"�@��y@�~�@��@��^@�`B@�&�@��u@�I�@�ƨ@��F@��P@�"�@�ȴ@��!@���@��+@�v�@�$�@�@�@���@�@���@���@��7@�X@�/@�V@���@��9@� �@��;@��w@��P@�t�@�S�@�K�@�K�@��@���@�^5@�=q@�x�@���@���@�bN@�I�@�z�@��@�j@�I�@�(�@��@��
@��w@���@�l�@�S�@�;d@�Q�@~�@u�@n{@e@^�R@U�T@PA�@F@A��@:�!@2�@+t�@&V@!��@@%@�
@�@@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
%B
�B
?}B
^5B
�oB
�qB
��B,BJ�B\)Bz�B�!B��B��BoB?}BE�BJ�B]/BiyBs�B�B�hB��B�!B�9B�FB�RB��B��B��B��B��B�B�;B�B�B�B��BB  B��B��B�B�fB��B�B��B�\B�Be`BD�BbNB_;BT�BL�B>wBN�BaHBgmBaHBXBy�B�LB�}BBB�}B�jB�RB�B�B��B��B�hB�=B�uB�=B|�Bk�B[#BL�B33B,B �BhBB�B�ZBȴB��B{�B[#B/B�B	7BB
��B
�TB
�wB
�?B
��B
��B
�B
q�B
gmB
Q�B
B�B
0!B
�B
1B	��B	�sB	��B	�LB	��B	�DB	z�B	k�B	aHB	[#B	R�B	N�B	G�B	=qB	.B	$�B	 �B	�B	�B	\B	+B��B��B�B�B�mB�BB�
B��B��BǮBƨBƨBŢBĜBĜBB��B�jB�LB�?B�-B�B��B��B��B��B�B�B��B��B��B��B��B��B��B�\B�\B�bB�JB�B�1B�\B�=B�+B�+B�B�B�+B�%B�B�B�B�B��B��B�hB�DB� Br�Bl�BffBbNBbNBcTBaHBaHBcTBffBhsBgmBgmBhsBiyBgmBe`BffBaHB_;B_;B_;BaHBcTBgmBhsBn�Bm�By�B�B�B�B�B�B�B�%B�+B�DB�VB�\B�hB��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�-B�?B�?B�LB�wB��BĜBÖBÖBŢB��B��B��B��B�
B�#B�)B�/B�;B�;B�NB�`B�mB�sB�yB�B�B��B��B��B��B	B	B	1B	JB	\B	oB	oB	{B	{B	�B	�B	�B	�B	�B	 �B	!�B	$�B	(�B	+B	/B	33B	7LB	7LB	8RB	:^B	>wB	?}B	A�B	E�B	F�B	H�B	J�B	O�B	XB	[#B	bNB	e`B	iyB	k�B	n�B	p�B	p�B	p�B	q�B	q�B	s�B	u�B	w�B	x�B	y�B	y�B	y�B	z�B	|�B	~�B	�B	�1B	�1B	�7B	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�3B	�?B	�LB	�LB	�XB	�^B	�^B	�dB	�qB	�wB	�wB	�wB	�}B	��B	��B	B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�#B	�B	�B	�#B	�/B	�5B	�BB	�NB	�TB	�TB	�TB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B

=B
DB
�B
!�B
(�B
,B
33B
9XB
;dB
@�B
F�B
J�B
S�B
\)B
aHB
gmB
l�B
p�B
u�B
x�B
z�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
/B
�B
?�B
^<B
�sB
�vB
��B,
BJ�B\(Bz�B�!B��B��BpB?|BE�BJ�B].BizBs�B�B�hB��B�"B�;B�JB�TB��B��B��B��B��B�B�9B�B�B�B��BB��B��B��B�B�iB��B�B��B�`B�Be`BD�BbMB_?BT�BL�B>uBN�BaGBgqBaDBXBy�B�MB�BBB�|B�jB�UB�B�B��B��B�iB�;B�tB�=B|�Bk�B[#BL�B33B,B �BiBB�B�YBȵB��B{�B[&B/B�B	9BB
��B
�VB
�}B
�BB
��B
��B
�B
q�B
gsB
Q�B
B�B
0)B
�B
8B	��B	�yB	��B	�SB	��B	�PB	z�B	k�B	aTB	[2B	R�B	N�B	G�B	=B	.$B	$�B	 �B	�B	�B	mB	=B�B��B�B��B�}B�UB�B��B��B��BƾBƸBŴBįBıB¥B��B�B�`B�UB�AB�/B�B��B��B�B�$B�B��B��B��B��B��B��B��B�rB�pB�wB�^B�3B�EB�tB�SB�AB�AB�/B�/B�CB�<B�5B�.B�B�B��B��B�}B�YB�Br�Bl�Bf}BbdBbfBckBa_Ba`BciBf|Bh�Bg�Bg�Bh�Bi�Bg�BevBfBa_B_QB_SB_QBa^BckBg�Bh�Bn�Bm�By�B�B�B�'B�5B�4B�4B�9B�BB�ZB�lB�oB�|B��B��B��B��B��B��B��B��B��B��B��B�B�2B�8B�>B�RB�SB�\B��B��BıBéBéBŲB��B��B��B��B�B�3B�<B�AB�KB�LB�]B�nB�B�B�B��B��B��B��B��B�B	B	B	@B	XB	lB	}B	}B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	)B	+B	/*B	3=B	7XB	7YB	8`B	:mB	>�B	?�B	A�B	E�B	F�B	H�B	J�B	O�B	XB	[/B	bZB	ejB	i�B	k�B	n�B	p�B	p�B	p�B	q�B	q�B	s�B	u�B	w�B	x�B	y�B	y�B	y�B	z�B	|�B	B	�#B	�<B	�;B	�BB	�nB	�nB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�1B	�4B	�4B	�5B	�;B	�EB	�UB	�UB	�_B	�eB	�gB	�pB	�yB	��B	��B	�B	��B	��B	��B	B	B	ÜB	ũB	ƱB	ǸB	ǷB	ȿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�0B	�+B	�%B	�B	�)B	�7B	�?B	�IB	�UB	�[B	�^B	�]B	�SB	�TB	�ZB	�\B	�bB	�gB	�mB	�mB	�vB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�B
 B
B
B
$B
$B
&B
'B
B
&B
*B
3B
8B
9B

DB
MB
�B
!�B
(�B
,B
39B
9[B
;hB
@�B
F�B
J�B
S�B
\,B
aKB
gqB
l�B
p�B
u�B
x�B
z�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214452016053112144520160531121445  AO  ARCAADJP                                                                    20151004021521    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151004021521  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151004021521  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121445  IP                  G�O�G�O�G�O�                