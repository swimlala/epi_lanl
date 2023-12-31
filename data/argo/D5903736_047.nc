CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:31Z AOML 3.0 creation; 2016-05-31T19:14:32Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230531  20160531121432  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               /A   AO  4051_7090_047                   2C  D   APEX                            5368                            041511                          846 @֩�T2 1   @֩�ޠ @5�C���d�1&�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    /A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�fD�3D�9�D���D���D�fD�FfD�s3D���D���D�@ D���DǼ�D�	�D�I�Dډ�D��fD��D�FfD�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�fD�3D�9�D���D���D�fD�FfD�s3D���D���D�@ D���DǼ�D�	�D�I�Dډ�D��fD��D�FfD�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A؃A�O�A�/A���Aղ-AՕ�AՅA�l�A�^5A�K�A�"�A�JA�1A���A��#AԬAԛ�Aԕ�Aԉ7A�hsA�=qA�/A��AЬA���A�%A�%A���AȬA��A���A���A�bA�C�A��A�t�A�A�"�A��A�E�A��`A��#A�oA�A�A���A��A���A�Q�A�"�A���A�9XA�I�A��+A��#A�VA���A�x�A�=qA��-A�Q�A�-A�  A��FA�bNA��HA�5?A���A�&�A�A��`A���A��jA���A���A��A�1'A��#A���A��`A��7A��hA�p�A�K�A��HA�v�A�?}A���A��FA�
=A���A�VA��jA�-A�ƨA�`BA��A��hA�%A��;A���A�oA���A�C�A���A���A��/A�hsA��mA�ZA���A��A�v�A��A���A�O�A�1'A�ĜA��A�G�A��A|�+Ax�DAw33At�yAs��An��Al�9Ak�FAj�Ai��Ail�Ah�Ag�^Ag�Ae�Ac��A_7LA]33AY��AWt�AU�TAT��AT��ATjAS��ASK�ARz�AP��AM�AJ��AI�FAI��AI�7AHbAG33AF(�AD��ADbABĜAA�A@�A?�#A?K�A?+A>v�A=��A=��A<�jA;��A:jA9O�A8��A81A7��A7t�A733A6�jA5�;A5A4��A4ZA3oA1XA0I�A0�A0�A/C�A-O�A+"�A)��A)VA&��A%l�A"I�A E�AdZA�-AI�A��A�9AM�A�
A�PA�A�+A�AAl�AO�A�jA�^A�7At�A�#AS�A��A�At�A%A
bNA	�7A��A��A�/A�!AA�A�AI�A��A��Az�A�;A �yA 1@���@�Ĝ@��w@�~�@��@���@�S�@�J@�w@�v�@�9@��@�C�@�-@�V@�I�@�C�@ꗍ@�Q�@��@�9@�@�z�@�bN@�b@��
@�dZ@�@��@ޗ�@�z�@��H@�ff@�X@ؓu@�1'@�K�@�E�@�I�@�@ҏ\@�^5@��@���@Ϯ@��@̼j@��H@ʇ+@ɲ-@�O�@�Z@�dZ@ź^@�@�bN@��;@��w@�33@���@�~�@�X@��@�%@�M�@��@���@�(�@��@��R@�V@�^5@���@��!@�5?@���@�Z@��;@�l�@�ȴ@��@�x�@�7L@��u@���@��!@��+@�~�@�-@��h@�9X@�K�@��@�ff@�5?@�@���@�`B@���@��@��F@�dZ@�+@��y@��R@�{@�x�@���@��9@�1@��@��
@���@�|�@�C�@�
=@�@��@�
=@�o@�C�@�C�@�
=@���@��@��-@�7L@��@�K�@�\)@�o@�
=@�"�@�t�@��@��@�o@�-@�@���@���@��@�@�V@��`@��D@��;@��
@�9X@��D@��u@�1'@�j@�A�@���@��T@�@��7@��7@��7@�x�@�O�@�`B@�X@�X@��h@���@���@��-@��^@��^@��^@���@���@��@���@�@�{@�J@��@���@�@��^@��^@��-@�p�@�p�@���@�O�@�hs@�?}@��9@���@�z�@�(�@��;@��F@�t�@�C�@��@���@��+@�V@�=q@�$�@�{@�@��@�G�@�G�@��@�r�@� �@��
@��@���@�\)@�;d@�+@�"�@�@���@���@��+@�ff@�E�@�@��7@�7L@�%@��/@��9@�Q�@��@�|�@�dZ@�dZ@�S�@�33@��@��H@��@���@�-@���@��^@�x�@�/@��/@���@�j@��w@�K�@�+@���@}@u�@l�j@e��@_\)@Wl�@P�9@G�@Bn�@<1@5��@1X@*��@$I�@E�@n�@v�@�@K�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A؃A�O�A�/A���Aղ-AՕ�AՅA�l�A�^5A�K�A�"�A�JA�1A���A��#AԬAԛ�Aԕ�Aԉ7A�hsA�=qA�/A��AЬA���A�%A�%A���AȬA��A���A���A�bA�C�A��A�t�A�A�"�A��A�E�A��`A��#A�oA�A�A���A��A���A�Q�A�"�A���A�9XA�I�A��+A��#A�VA���A�x�A�=qA��-A�Q�A�-A�  A��FA�bNA��HA�5?A���A�&�A�A��`A���A��jA���A���A��A�1'A��#A���A��`A��7A��hA�p�A�K�A��HA�v�A�?}A���A��FA�
=A���A�VA��jA�-A�ƨA�`BA��A��hA�%A��;A���A�oA���A�C�A���A���A��/A�hsA��mA�ZA���A��A�v�A��A���A�O�A�1'A�ĜA��A�G�A��A|�+Ax�DAw33At�yAs��An��Al�9Ak�FAj�Ai��Ail�Ah�Ag�^Ag�Ae�Ac��A_7LA]33AY��AWt�AU�TAT��AT��ATjAS��ASK�ARz�AP��AM�AJ��AI�FAI��AI�7AHbAG33AF(�AD��ADbABĜAA�A@�A?�#A?K�A?+A>v�A=��A=��A<�jA;��A:jA9O�A8��A81A7��A7t�A733A6�jA5�;A5A4��A4ZA3oA1XA0I�A0�A0�A/C�A-O�A+"�A)��A)VA&��A%l�A"I�A E�AdZA�-AI�A��A�9AM�A�
A�PA�A�+A�AAl�AO�A�jA�^A�7At�A�#AS�A��A�At�A%A
bNA	�7A��A��A�/A�!AA�A�AI�A��A��Az�A�;A �yA 1@���@�Ĝ@��w@�~�@��@���@�S�@�J@�w@�v�@�9@��@�C�@�-@�V@�I�@�C�@ꗍ@�Q�@��@�9@�@�z�@�bN@�b@��
@�dZ@�@��@ޗ�@�z�@��H@�ff@�X@ؓu@�1'@�K�@�E�@�I�@�@ҏ\@�^5@��@���@Ϯ@��@̼j@��H@ʇ+@ɲ-@�O�@�Z@�dZ@ź^@�@�bN@��;@��w@�33@���@�~�@�X@��@�%@�M�@��@���@�(�@��@��R@�V@�^5@���@��!@�5?@���@�Z@��;@�l�@�ȴ@��@�x�@�7L@��u@���@��!@��+@�~�@�-@��h@�9X@�K�@��@�ff@�5?@�@���@�`B@���@��@��F@�dZ@�+@��y@��R@�{@�x�@���@��9@�1@��@��
@���@�|�@�C�@�
=@�@��@�
=@�o@�C�@�C�@�
=@���@��@��-@�7L@��@�K�@�\)@�o@�
=@�"�@�t�@��@��@�o@�-@�@���@���@��@�@�V@��`@��D@��;@��
@�9X@��D@��u@�1'@�j@�A�@���@��T@�@��7@��7@��7@�x�@�O�@�`B@�X@�X@��h@���@���@��-@��^@��^@��^@���@���@��@���@�@�{@�J@��@���@�@��^@��^@��-@�p�@�p�@���@�O�@�hs@�?}@��9@���@�z�@�(�@��;@��F@�t�@�C�@��@���@��+@�V@�=q@�$�@�{@�@��@�G�@�G�@��@�r�@� �@��
@��@���@�\)@�;d@�+@�"�@�@���@���@��+@�ff@�E�@�@��7@�7L@�%@��/@��9@�Q�@��@�|�@�dZ@�dZ@�S�@�33@��@��H@��@���@�-@���@��^@�x�@�/@��/@���@�j@��w@�K�@�+@���@}@u�@l�j@e��@_\)@Wl�@P�9@G�@Bn�@<1@5��@1X@*��@$I�@E�@n�@v�@�@K�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�BB\B\B\BoBuBuB�B�B�B �B!�B!�B"�B#�B#�B#�B#�B"�B�B�BB�5B�}B�B��B��B��B��B��B�B�'B�!B�!B�B��B��B�{B�7B�+Bo�BS�BYBjBXBT�BaHBffBq�Br�Bm�BgmB`BBZBXBVBR�BK�BH�BG�BE�BC�B?}B;dB6FB33B.B-B.B-B,B,B+B)�B'�B$�B �B{B��B�B�B�B�yB�mB�sB�TB�5B��B�?B��B� Bt�Bl�Be`B\)BT�BI�B49BhB  B��B�B�mB�BÖB�XB��Bp�B-B�BuB
=B
�B
�}B
��B
��B
q�B
W
B
N�B
8RB
�B
uB
B	��B	�)B	��B	ǮB	��B	�jB	�dB	�LB	�!B	�B	��B	�uB	|�B	o�B	`BB	VB	P�B	M�B	M�B	K�B	H�B	D�B	?}B	7LB	$�B	�B	�B	�B	�B	oB	JB	%B	B��B��B��B��B��B��B�B�B�B�yB�mB�NB�B�B�B�B��B��B��B��B��B��B��B��BȴBB�}B�}B�qB�^B�3B�B��B��B��B��B�hB�JB�DB�VB�VB�JB�7B�1B�%B�B� B}�B{�Bz�By�Bw�Bt�Bo�Bk�BffBdZBcTBbNBaHBaHB_;B^5B^5B^5B]/B]/B\)B[#BXBYBXBW
BT�BS�BT�BVBXBYB[#B]/B]/B^5B_;B`BBbNBcTBe`Be`BffBgmBiyBk�Bk�BiyBhsBiyBjBjBjBjBjBiyBhsBiyBjBm�Br�Bu�Bv�Bx�By�By�B{�B}�B�B�%B�%B�+B�+B�DB�bB�bB��B��B��B��B��B��B��B��B��B�-B�?B�FB�RB�dB�jB�wBĜB��B�wB�wB�wB�}B��BÖB��B��B��B�B�)B�HB�TB�`B�fB�yB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	+B	PB	\B	hB	uB	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	'�B	)�B	/B	1'B	33B	;dB	>wB	@�B	B�B	D�B	I�B	M�B	N�B	N�B	T�B	YB	]/B	aHB	cTB	e`B	k�B	p�B	r�B	r�B	q�B	q�B	r�B	s�B	t�B	x�B	|�B	|�B	� B	�B	�+B	�JB	�bB	�hB	�hB	�{B	��B	��B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�LB	�RB	�RB	�XB	�dB	�jB	�}B	��B	��B	��B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
VB
�B
�B
%�B
-B
49B
:^B
;dB
?}B
E�B
K�B
P�B
XB
]/B
bNB
gmB
jB
o�B
r�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B&BdBaBdByB�B�B�B�B�B �B!�B!�B"�B#�B#�B#�B#�B"�B�B�BB�:B��B�!B��B��B��B��B�B�B�-B�$B�)B�B��B��B��B�:B�3Bo�BS�BYBj�BXBUBaLBfhBq�Br�Bm�BgrB`FBZ"BXBVBR�BK�BH�BG�BE�BC�B?�B;gB6KB33B.B-B.B-B,B,B+B)�B'�B$�B �B{B��B�B�B�B�zB�oB�wB�XB�4B��B�AB��B�Bt�Bl�Be`B\+BT�BI�B49BkB  B��B�B�mB�BÖB�WB��Bp�B-B�BvB
>B
�B
�B
��B
��B
q�B
WB
N�B
8WB
�B
�B
B	��B	�/B	��B	ǸB	��B	�sB	�mB	�YB	�-B	�B	��B	��B	|�B	o�B	`OB	VB	P�B	M�B	M�B	K�B	H�B	D�B	?�B	7[B	$�B	�B	�B	�B	�B	�B	[B	4B	B�B�
B��B��B��B��B��B�B�B�B�B�aB�1B�B�B�B�B�B�B�B�B��B��B��B��B£B��B��B��B�qB�HB�"B��B��B��B��B�}B�_B�YB�jB�kB�`B�LB�EB�8B�/B�B~B{�Bz�By�Bw�Bt�Bo�Bk�Bf�BdoBcjBbgBabBa]B_TB^NB^MB^MB]DB]EB\AB[=BX&BY.BX&BW#BUBTBUBVBX)BY.B[;B]DB]HB^LB_RB`ZBbfBcmBewBexBf}Bg�Bi�Bk�Bk�Bi�Bh�Bi�Bj�Bj�Bj�Bj�Bj�Bi�Bh�Bi�Bj�Bm�Br�Bu�Bv�Bx�By�By�B{�B~	B�'B�:B�9B�?B�>B�XB�wB�vB��B��B��B��B��B��B��B��B�B�@B�SB�[B�dB�wB�{B��BĭB��B��B��B��B��B��BéB��B��B�B�'B�8B�ZB�dB�pB�tB�B�B�B��B�B�B�B�B�B�B�B��B��B��B�B�B	 B	B	B	9B	`B	lB	wB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	'�B	*B	/&B	13B	3>B	;qB	>�B	@�B	B�B	D�B	I�B	M�B	N�B	N�B	U	B	Y#B	]8B	aTB	c`B	ekB	k�B	p�B	r�B	r�B	q�B	q�B	r�B	s�B	t�B	x�B	|�B	|�B	�
B	�*B	�4B	�UB	�mB	�sB	�rB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�%B	�"B	�!B	�2B	�@B	�OB	�TB	�\B	�ZB	�`B	�mB	�tB	��B	��B	��B	��B	��B	ÞB	ĤB	ūB	ưB	ǷB	ǸB	ȻB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�$B	�$B	�$B	�,B	�0B	�5B	�8B	�4B	�BB	�JB	�NB	�TB	�UB	�[B	�`B	�aB	�aB	�aB	�kB	�sB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
ZB
�B
�B
%�B
-B
4>B
:cB
;hB
?�B
E�B
K�B
P�B
XB
]4B
bSB
grB
j�B
o�B
r�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214322016053112143220160531121432  AO  ARCAADJP                                                                    20140721230531    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230531  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230531  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121432  IP                  G�O�G�O�G�O�                