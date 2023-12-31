CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:16Z AOML 3.0 creation; 2016-05-31T19:14:27Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230516  20160531121427  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_019                   2C  D   APEX                            5368                            041511                          846 @�b(�?�1   @�b(�� @3��z�H�dpj~��#1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyl�D�fD�<�D���D��3D�3D�33D��3D�ɚD�	�D�I�D��fD�� D��D�FfD�|�D���D� D�C3D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyl�D�fD�<�D���D��3D�3D�33D��3D�ɚD�	�D�I�D��fD�� D��D�FfD�|�D���D� D�C3D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��;A��TA��TA��`A��`A��`A��mA��TA��`A���Aإ�A؁A�~�A�z�A�x�A�v�A�v�A�p�A�n�A�n�A�l�A�l�A�l�A�jA�dZA�\)A�ZA�XA�Q�A�O�A�G�A��A�9XAև+A��mAհ!AլAե�A�C�A�O�A�(�A��A���A��AΩ�A�=qA��TA˥�A�1A�(�A�r�A�%A� �A�5?A��HA�jA��A���A��wA��;A�n�A���A� �A���A�|�A�A�A�E�A��!A�VA��PA�t�A�;dA�VA�&�A��hA��A�K�A���A���A��!A��uA�VA��
A���A���A��TA�ĜA���A��A��HA�bNA�n�A��mA�ĜA��A� �A�(�A��
A�Q�A�ȴA��A��yA�^5A��mA�dZA�~�A���A���A��A�M�A��+A�p�A�VA}��AzA�Ax�RAwt�Avr�As�-Apn�Alr�Aj��Ai?}Ag7LAbE�A^5?A\�A\{AY33AVn�AT�jASK�AR��ARffAQ�wAO��AM�wAL1'AK/AJJAHI�AFVAD�!A@�A?�PA>�A=��A:��A8�/A7��A7C�A6�A6ZA5��A4ĜA3�A2^5A0�A,��A+|�A(��A&�!A&VA&9XA%�PA%`BA%�A$Q�A#�;A#��A"�A!��A!VA�A5?A�A�jA�A�PA�9Ap�A�\AI�AhsA�uA�7An�A��A?}AjA��Ax�A�9A(�Al�A|�A
n�A	l�A�RA��A�9A5?AS�A�yA�jAt�A �y@�;d@��j@�V@��`@��w@��F@���@�C�@�hs@���@�-@�1@�j@��@�5?@���@��#@��@��
@��m@�x�@�b@ە�@�$�@���@�ƨ@�@��
@���@���@Л�@�\)@�{@�|�@���@��
@�ȴ@��#@�V@��@��m@��;@�  @þw@��@�;d@��@���@�C�@��+@��@�"�@�S�@�dZ@�S�@��!@�/@�O�@�1'@��@�-@���@���@���@��7@�7L@�%@�j@�C�@��H@�~�@���@�?}@��`@��@�z�@�+@���@�{@�x�@���@�I�@� �@���@�dZ@�C�@�@��+@�-@�@��T@���@���@��@�?}@���@��/@���@�bN@��@��w@��P@�"�@���@�n�@�-@��#@��^@�p�@�7L@�&�@�V@���@�Z@��;@��
@��w@���@�dZ@�33@�
=@��H@���@���@�=q@��7@�p�@��@��`@���@��9@�I�@�9X@�b@��@��F@��@�K�@�o@��@�@�@��y@���@���@���@�^5@�$�@��@���@�?}@���@���@��@��@�bN@�b@��@�\)@�+@��@���@�=q@��#@���@�`B@�&�@���@��/@�1'@�ƨ@���@�l�@�\)@�S�@��@�~�@�5?@�$�@�@��@���@��^@�`B@�/@�V@��/@��@��u@��@�I�@�b@��
@���@���@�|�@�C�@�@��y@�ff@��T@��^@���@���@�`B@�G�@�&�@���@��`@��@�z�@�Q�@�(�@� �@�b@���@���@��@�"�@�
=@��R@�v�@�-@��T@��^@��-@��7@�`B@�V@���@��9@��9@��@���@��u@�r�@�  @��
@���@�ƨ@��F@���@���@���@���@���@��@�S�@��@�ȴ@��\@�v�@�n�@�^5@�V@�M�@�E�@�5?@���@�@�?}@��@���@�Ĝ@��9@���@���@�Z@�A�@��m@�|�@�K�@�"�@�o@�@��y@���@��+@�=q@�J@��#@���@|z�@r=q@g�w@_�;@W��@LZ@D��@>5?@8bN@3��@/l�@+ƨ@)%@$�@��@�@�D@%@V@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��;A��TA��TA��`A��`A��`A��mA��TA��`A���Aإ�A؁A�~�A�z�A�x�A�v�A�v�A�p�A�n�A�n�A�l�A�l�A�l�A�jA�dZA�\)A�ZA�XA�Q�A�O�A�G�A��A�9XAև+A��mAհ!AլAե�A�C�A�O�A�(�A��A���A��AΩ�A�=qA��TA˥�A�1A�(�A�r�A�%A� �A�5?A��HA�jA��A���A��wA��;A�n�A���A� �A���A�|�A�A�A�E�A��!A�VA��PA�t�A�;dA�VA�&�A��hA��A�K�A���A���A��!A��uA�VA��
A���A���A��TA�ĜA���A��A��HA�bNA�n�A��mA�ĜA��A� �A�(�A��
A�Q�A�ȴA��A��yA�^5A��mA�dZA�~�A���A���A��A�M�A��+A�p�A�VA}��AzA�Ax�RAwt�Avr�As�-Apn�Alr�Aj��Ai?}Ag7LAbE�A^5?A\�A\{AY33AVn�AT�jASK�AR��ARffAQ�wAO��AM�wAL1'AK/AJJAHI�AFVAD�!A@�A?�PA>�A=��A:��A8�/A7��A7C�A6�A6ZA5��A4ĜA3�A2^5A0�A,��A+|�A(��A&�!A&VA&9XA%�PA%`BA%�A$Q�A#�;A#��A"�A!��A!VA�A5?A�A�jA�A�PA�9Ap�A�\AI�AhsA�uA�7An�A��A?}AjA��Ax�A�9A(�Al�A|�A
n�A	l�A�RA��A�9A5?AS�A�yA�jAt�A �y@�;d@��j@�V@��`@��w@��F@���@�C�@�hs@���@�-@�1@�j@��@�5?@���@��#@��@��
@��m@�x�@�b@ە�@�$�@���@�ƨ@�@��
@���@���@Л�@�\)@�{@�|�@���@��
@�ȴ@��#@�V@��@��m@��;@�  @þw@��@�;d@��@���@�C�@��+@��@�"�@�S�@�dZ@�S�@��!@�/@�O�@�1'@��@�-@���@���@���@��7@�7L@�%@�j@�C�@��H@�~�@���@�?}@��`@��@�z�@�+@���@�{@�x�@���@�I�@� �@���@�dZ@�C�@�@��+@�-@�@��T@���@���@��@�?}@���@��/@���@�bN@��@��w@��P@�"�@���@�n�@�-@��#@��^@�p�@�7L@�&�@�V@���@�Z@��;@��
@��w@���@�dZ@�33@�
=@��H@���@���@�=q@��7@�p�@��@��`@���@��9@�I�@�9X@�b@��@��F@��@�K�@�o@��@�@�@��y@���@���@���@�^5@�$�@��@���@�?}@���@���@��@��@�bN@�b@��@�\)@�+@��@���@�=q@��#@���@�`B@�&�@���@��/@�1'@�ƨ@���@�l�@�\)@�S�@��@�~�@�5?@�$�@�@��@���@��^@�`B@�/@�V@��/@��@��u@��@�I�@�b@��
@���@���@�|�@�C�@�@��y@�ff@��T@��^@���@���@�`B@�G�@�&�@���@��`@��@�z�@�Q�@�(�@� �@�b@���@���@��@�"�@�
=@��R@�v�@�-@��T@��^@��-@��7@�`B@�V@���@��9@��9@��@���@��u@�r�@�  @��
@���@�ƨ@��F@���@���@���@���@���@��@�S�@��@�ȴ@��\@�v�@�n�@�^5@�V@�M�@�E�@�5?@���@�@�?}@��@���@�Ĝ@��9@���@���@�Z@�A�@��m@�|�@�K�@�"�@�o@�@��y@���@��+@�=q@�J@��#@���@|z�@r=q@g�w@_�;@W��@LZ@D��@>5?@8bN@3��@/l�@+ƨ@)%@$�@��@�@�D@%@V@	x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�9BVBs�B��BĜB��B�B�NB�ZB�HB�B��B��B�3B�B�B��B�JB�Bv�BgmBT�B,B�B�B��B�B0!B.B+B(�B+B(�B%�B&�B&�B$�B"�B�B�B�B\B+B��B�NBȴB�^B�B�VBp�BdZBaHBW
B;dB�BB�B��B�wB�3B��B�bB�%BgmBVBL�B>wB-B�B�BJB
��B
��B
�B
�ZB
�
B
��B
�9B
��B
��B
�7B
u�B
[#B
6FB
$�B
�B
�B
bB
B	�B	�B	��B	ĜB	�jB	��B	�JB	�7B	�B	x�B	hsB	^5B	[#B	YB	W
B	R�B	K�B	C�B	=qB	9XB	33B	+B	"�B	�B	VB		7B	B��B��B�B�B�B�B�sB�`B�HB�)B�B��BǮBB�jB�RB�LB�?B�?B�FB�^B�RB�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B�uB�oB�hB�VB�VB�\B�uB�hB�VB�DB�DB�7B�Bx�Bs�Bs�Bs�Bv�Bs�Br�Bs�Bv�Bw�Bu�Bo�BjBffBdZBbNBcTBdZBcTBbNBaHB_;B]/BYBVBW
B[#B_;B[#B`BB_;BYBYB]/B^5B`BB`BBe`BgmBiyBk�Br�Br�Bv�Bt�Bs�Br�Br�Bs�Br�Bv�B� B�+B�=B�PB�uB�JB�1B�%B�B�B�=B�bB�{B��B��B��B��B��B��B�B�XB�}B��BBÖBƨBƨBǮB��B��B��B��B�B�B�)B�5B�5B�fB�B�B��B��B	B	B		7B	JB	VB	bB	�B	�B	�B	�B	 �B	#�B	%�B	(�B	+B	,B	.B	0!B	5?B	7LB	;dB	@�B	E�B	G�B	I�B	M�B	O�B	R�B	S�B	T�B	VB	VB	ZB	`BB	aHB	cTB	dZB	ffB	hsB	jB	k�B	l�B	m�B	p�B	u�B	v�B	y�B	z�B	{�B	{�B	}�B	~�B	}�B	}�B	� B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�DB	�JB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�FB	�LB	�XB	�XB	�XB	�wB	��B	B	B	ÖB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�)B	�BB	�BB	�BB	�BB	�BB	�BB	�;B	�BB	�BB	�BB	�NB	�NB	�NB	�TB	�ZB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
PB
VB
�B
!�B
(�B
.B
/B
49B
9XB
>wB
D�B
I�B
N�B
R�B
VB
[#B
bNB
hsB
n�B
s�B
w�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�:BZBs�B��BģB��B�%B�TB�cB�RB�B��B��B�8B�!B�B��B�PB�Bv�BgtBUB,B�B�B��B�B0$B.B+B(�B+B(�B%�B&�B&�B$�B"�B�B�B�B^B,B��B�QBȹB�_B�B�ZBp�Bd[BaGBWB;dB�B
B�B��B�xB�0B��B�bB�&BgnBVBL�B>zB-B�B�BLB
��B
��B
�B
�YB
�B
��B
�>B
��B
��B
�<B
u�B
[(B
6NB
$�B
�B
�B
jB
B	�B	�B	��B	ħB	�vB	��B	�WB	�AB	�)B	x�B	h�B	^CB	[0B	Y#B	WB	SB	K�B	C�B	=�B	9fB	3BB	+B	"�B	�B	hB		HB	1B�B��B�B��B�B�B�B�pB�\B�<B�#B��B��B¤B��B�dB�aB�SB�TB�[B�rB�dB�LB�AB�0B�B�	B��B��B��B��B��B��B��B��B��B��B��B�~B�jB�iB�sB��B�}B�jB�YB�YB�NB�)Bx�Bs�Bs�Bs�Bv�Bs�Br�Bs�Bv�Bw�Bu�Bo�Bj�Bf|BdqBbeBckBdpBcjBbeBa_B_TB]EBY.BVBW B[:B_PB[:B`[B_RBY+BY,B]FB^LB`WB`YBevBg�Bi�Bk�Br�Br�Bv�Bt�Bs�Br�Br�Bs�Br�Bv�B�B�BB�RB�cB��B�]B�FB�:B�.B�.B�OB�wB��B��B��B��B��B��B��B�B�hB��B��B¢BéBƷBƸBǾB��B��B��B�B�B�-B�;B�EB�CB�xB��B�B��B�
B	"B	(B		FB	VB	eB	qB	�B	�B	�B	�B	 �B	#�B	%�B	) B	+B	,B	.!B	0.B	5MB	7XB	;oB	@�B	E�B	G�B	I�B	M�B	O�B	R�B	TB	U	B	VB	VB	Z,B	`KB	aSB	caB	deB	fpB	h�B	j�B	k�B	l�B	m�B	p�B	u�B	v�B	y�B	z�B	{�B	{�B	}�B	B	}�B	}�B	�B	�B	�B	�&B	�.B	�4B	�;B	�>B	�HB	�MB	�UB	�eB	�pB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�0B	�=B	�OB	�UB	�aB	�`B	�aB	��B	��B	B	B	ÜB	ĦB	ūB	ȻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�2B	�JB	�KB	�IB	�JB	�IB	�IB	�BB	�KB	�KB	�HB	�TB	�TB	�RB	�\B	�aB	�nB	�vB	�zB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B
 B
B
B
B
B
B
B
B
B
B
B
B
B
'B
&B
&B
$B
%B
%B
%B
,B
2B
6B
8B
	<B

DB

BB

CB

EB
LB
IB
PB
QB
VB
\B
�B
!�B
(�B
.B
/!B
4?B
9\B
>|B
D�B
I�B
N�B
R�B
VB
[&B
bPB
hvB
n�B
s�B
w�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214272016053112142720160531121427  AO  ARCAADJP                                                                    20140721230516    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230516  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230516  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121427  IP                  G�O�G�O�G�O�                