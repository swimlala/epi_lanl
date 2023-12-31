CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-13T09:15:35Z AOML 3.0 creation; 2016-08-07T21:51:30Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160613091535  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_127                   2C  D   APEX                            6529                            072314                          846 @׳��_7s1   @׳�O� P@0��hr�!�d�V�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�ffB�  B���C   C  C  C  C�C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,�C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` DyfD��D�I�D��fD��fD��D�I�D��fD��3D�� D�0 D��fD��fD�3D�6fDڙ�D�ɚD���D�S3D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Y��@�  @�  A  A(  AH  Ah  A�  A�  A�  A�  A�  A�  A�  A�  B  B
  B  B  B"  B*  B2  B:  BB  BJ  BR  BZ  Bb  Bi��Br  Bz  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�ffB�  B���C � C� C� C� C��C
� CffC� C� C� C� C� C� C� C� C� C � C"� C$� C&��C(� C*� C,��C.� C0ffC2� C4� C6� C8� C:� C<� C>� C@� CB� CD� CF� CH� CJ� CL� CN� CP� CR� CT��CV� CX� CZ� C\� C^� C`� Cb� Cd� Cf� Ch� Cj� Cl� Cn� Cp� Cr� Ct� Cv� Cx� Cz� C|� C~� C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D��D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB�DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy&fD�,�D�Y�D��fD��fD�)�D�Y�D��fD��3D�  D�@ D��fD��fD�3D�FfDک�D�ٚD�	�D�c3D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A�z�A�E�A�oAڶFAڝ�Aڏ\A�v�A�E�A�JA��;A�ƨAٸRAُ\A�bNA�S�A�E�A�=qA��A��#A؁A�A�ffA�VA֕�A�^5A��A� �A���A��A��A��AլA�|�A�\)A�5?A���A��HAԺ^A�n�A�A�A�(�A�
=A���A��`A�-Aѧ�A�oA�hsA�Q�A�=qA�v�A�JA�jA���A�JA�33Aƴ9A�l�A���A�?}A���A��mA��7A��\A�&�A��A�(�A�"�A��-A�z�A��A��A���A���A��HA�l�A��-A�A���A���A�ƨA��A�ZA�VA���A�+A��A��wA��^A���A��A�=qA��A�x�A��wA�S�A���A���A��PA��A�ȴA?}A}��Ay�-Ar�\Aox�Al$�Af�+Ab�A\-AY��AY�AX��AU�AP�AN1'AM\)AL�AL^5AK33AJ5?AHZAF�jAE��AD  ACO�AC+AC/AC7LAC;dAC+AA�PA@��A@�A?��A>A�A<��A;;dA:9XA9%A8A7K�A7oA6bA3/A0jA.$�A+\)A(bNA%"�A#�TA"�A �`A�+A|�AA�A
=A��AI�A�uA�+Ax�AffA��A-A��A�9A�yAVAG�A��A�^A/A
��A
�!A
bNA
�A	��A	��A	?}A�RA�A��Al�AK�A&�A�`AĜA�Az�A��A/A�A�A�A;dA�AȴAQ�A�A�FA�PA|�AhsA/A�HA�uA$�A��AK�A ��A VA 1@�33@�p�@���@��@��+@�ff@�{@��-@��@���@���@� �@�S�@��H@���@��T@��@�Z@�w@�P@�"�@�@�V@� �@�F@�C�@���@�R@�v�@���@�?}@�@�ff@ꟾ@�!@�^5@��@��#@��@�hs@��@�|�@���@��@�p�@���@�@�@�Z@�(�@�1@�@��@�J@�V@��y@�ff@�$�@�$�@�@��@�E�@�^5@�^5@�M�@���@�x�@�`B@�?}@�&�@�V@��`@���@ܣ�@ۍP@�K�@�"�@��y@�E�@�p�@��@���@ش9@��
@��@�/@�1'@ӕ�@�S�@�;d@�ȴ@�M�@�{@�$�@�^5@�O�@��
@��@��T@�G�@�/@���@̛�@̓u@�r�@�1@ʟ�@��T@ɑh@Ȭ@�l�@Ɨ�@���@ǅ@��@�@�ƨ@��;@�Q�@�bN@Å@�M�@�5?@��@�&�@���@��@���@�S�@�;d@�"�@��\@���@���@�`B@���@��/@��/@��`@��@�9X@���@�ƨ@�S�@�ȴ@�-@�`B@��9@��D@�j@�Q�@�b@��;@�t�@��y@�ȴ@���@���@�V@���@�O�@�/@�V@���@��D@��@��y@�v�@�J@���@��h@���@�9X@�l�@���@��+@�V@�=q@�$�@�{@��@��T@��#@���@��^@�hs@�X@��@���@���@�A�@��F@��@���@�@���@�t�@��@�^5@��-@���@�r�@�I�@�9X@� �@� �@� �@��@�b@�ƨ@���@�@���@���@��h@�x�@�?}@��@�V@�V@��`@�bN@��@�1@��m@��w@��@���@��+@�V@�5?@�J@��h@�%@���@�Ĝ@��@���@�z�@�1'@�K�@��@���@�=q@���@�p�@��@���@��m@���@�t�@�33@�o@��y@��!@�ff@��@���@�/@�Ĝ@�r�@�bN@�bN@�(�@��m@���@�"�@���@���@�E�@�A�@��@�A�@~ȴ@w;d@k"�@a�@W��@O+@J�@D��@;ƨ@4�D@.ff@(�u@"�\@��@M�@z�@hs@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A��A��A�z�A�E�A�oAڶFAڝ�Aڏ\A�v�A�E�A�JA��;A�ƨAٸRAُ\A�bNA�S�A�E�A�=qA��A��#A؁A�A�ffA�VA֕�A�^5A��A� �A���A��A��A��AլA�|�A�\)A�5?A���A��HAԺ^A�n�A�A�A�(�A�
=A���A��`A�-Aѧ�A�oA�hsA�Q�A�=qA�v�A�JA�jA���A�JA�33Aƴ9A�l�A���A�?}A���A��mA��7A��\A�&�A��A�(�A�"�A��-A�z�A��A��A���A���A��HA�l�A��-A�A���A���A�ƨA��A�ZA�VA���A�+A��A��wA��^A���A��A�=qA��A�x�A��wA�S�A���A���A��PA��A�ȴA?}A}��Ay�-Ar�\Aox�Al$�Af�+Ab�A\-AY��AY�AX��AU�AP�AN1'AM\)AL�AL^5AK33AJ5?AHZAF�jAE��AD  ACO�AC+AC/AC7LAC;dAC+AA�PA@��A@�A?��A>A�A<��A;;dA:9XA9%A8A7K�A7oA6bA3/A0jA.$�A+\)A(bNA%"�A#�TA"�A �`A�+A|�AA�A
=A��AI�A�uA�+Ax�AffA��A-A��A�9A�yAVAG�A��A�^A/A
��A
�!A
bNA
�A	��A	��A	?}A�RA�A��Al�AK�A&�A�`AĜA�Az�A��A/A�A�A�A;dA�AȴAQ�A�A�FA�PA|�AhsA/A�HA�uA$�A��AK�A ��A VA 1@�33@�p�@���@��@��+@�ff@�{@��-@��@���@���@� �@�S�@��H@���@��T@��@�Z@�w@�P@�"�@�@�V@� �@�F@�C�@���@�R@�v�@���@�?}@�@�ff@ꟾ@�!@�^5@��@��#@��@�hs@��@�|�@���@��@�p�@���@�@�@�Z@�(�@�1@�@��@�J@�V@��y@�ff@�$�@�$�@�@��@�E�@�^5@�^5@�M�@���@�x�@�`B@�?}@�&�@�V@��`@���@ܣ�@ۍP@�K�@�"�@��y@�E�@�p�@��@���@ش9@��
@��@�/@�1'@ӕ�@�S�@�;d@�ȴ@�M�@�{@�$�@�^5@�O�@��
@��@��T@�G�@�/@���@̛�@̓u@�r�@�1@ʟ�@��T@ɑh@Ȭ@�l�@Ɨ�@���@ǅ@��@�@�ƨ@��;@�Q�@�bN@Å@�M�@�5?@��@�&�@���@��@���@�S�@�;d@�"�@��\@���@���@�`B@���@��/@��/@��`@��@�9X@���@�ƨ@�S�@�ȴ@�-@�`B@��9@��D@�j@�Q�@�b@��;@�t�@��y@�ȴ@���@���@�V@���@�O�@�/@�V@���@��D@��@��y@�v�@�J@���@��h@���@�9X@�l�@���@��+@�V@�=q@�$�@�{@��@��T@��#@���@��^@�hs@�X@��@���@���@�A�@��F@��@���@�@���@�t�@��@�^5@��-@���@�r�@�I�@�9X@� �@� �@� �@��@�b@�ƨ@���@�@���@���@��h@�x�@�?}@��@�V@�V@��`@�bN@��@�1@��m@��w@��@���@��+@�V@�5?@�J@��h@�%@���@�Ĝ@��@���@�z�@�1'@�K�@��@���@�=q@���@�p�@��@���@��m@���@�t�@�33@�o@��y@��!@�ff@��@���@�/@�Ĝ@�r�@�bN@�bN@�(�@��m@���@�"�@���@���G�O�@�A�@��@�A�@~ȴ@w;d@k"�@a�@W��@O+@J�@D��@;ƨ@4�D@.ff@(�u@"�\@��@M�@z�@hs@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBVBVBVBVBPBDB
=B	7B1B%BBBB  BB  B  B
��B
��B
��B
��B
��B
��B
��B
�B
�sB
�5B
�B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
ɺB
ŢB
ÖB
B
�wB
�qB
�dB
�dB
�LB
��B
��B
��B
��B
��B
�!B
�}B
�mB
�B1B49BR�Bl�B�bB��B�yB��B{B-B49B33B.B�BoBbB
=B��B�B�fB�ZB�HB�BȴB�B��B��B��B�uB�1Bk�BC�B=qB8RB7LB6FB5?B(�B%B
��B
�HB
�B
��B
�-B
��B
�PB
x�B
m�B
G�B
6FB
+B
JB	�NB	��B	�RB	�uB	x�B	XB	M�B	I�B	B�B	6FB	!�B	oB	JB	1B	B��B��B�B�NB�5B�B�#B�ZB�fB�mB�mB�yB�yB�mB�fB�NB�)B�B��B��B��B��B��B��B��B��BǮB��B�qB�XB�^B�RB�LB�?B�3B�-B�-B�3B�3B�-B�'B�B��B��B��B��B��B��B�!B�dB�B��B��B	B	B	B	B	B	DB	�B	�B	�B	�B	"�B	"�B	"�B	!�B	!�B	!�B	 �B	 �B	�B	�B	�B	#�B	(�B	,B	1'B	2-B	49B	8RB	;dB	<jB	<jB	<jB	=qB	=qB	=qB	<jB	=qB	J�B	T�B	S�B	R�B	Q�B	Q�B	ZB	dZB	iyB	l�B	m�B	n�B	q�B	v�B	x�B	z�B	}�B	�B	�B	�B	�%B	�JB	�JB	�\B	�bB	�hB	��B	��B	��B	��B	��B	�B	�B	�!B	�FB	�dB	�^B	�}B	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�/B	�/B	�/B	�/B	�/B	�/B	�)B	�B	��B	��B	��B	��B	��B	��B	�B	�)B	�;B	�BB	�HB	�HB	�BB	�HB	�NB	�HB	�HB	�HB	�TB	�ZB	�ZB	�HB	�BB	�BB	�;B	�/B	�)B	�/B	�/B	�/B	�;B	�BB	�NB	�NB	�NB	�TB	�TB	�NB	�HB	�HB	�HB	�NB	�HB	�HB	�NB	�fB	�fB	�`B	�`B	�ZB	�ZB	�TB	�NB	�HB	�;B	�5B	�)B	�#B	�/B	�;B	�B	�B	�mB	�;B	�NB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
%B
%B
%B
%B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
B
B
B
%B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
	7B
	7B
	7B

=B
DB
DB
DB
JB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
{B
{B
"�B
(�B
.B
2-B
5?B
<jB
B�B
G�B
J�B
O�B
T�B
\)B
aHB
ffB
k�B
p�B
t�B
{�B
~�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B2B1B2B3B+BB
B	BB B�B�B �B
��B �B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�MB
�B
��B
��B
��B
��B
��B
ϻB
��B
��B
��B
ͱB
ʞB
ʞB
ɕB
�|B
�pB
�kB
�SB
�OB
�>B
�@B
�(B
��B
��B
��B
��B
��B
��B
�ZB
�IB
�BB4BR�BleB�<B��B�OB��BOB,�B4B3B-�B|BAB3B
B��B�cB�<B�-B�B��BȃB��B�eB�VB�TB�EB�BkVBChB=DB8"B7B6B5B(�B�B
�B
�B
��B
ͤB
��B
�sB
�!B
x�B
mdB
G�B
6B
*�B
B	� B	ͪB	�'B	�KB	x�B	W�B	M�B	I�B	BjB	6B	!�B	IB	%B	B	�B��B��B�bB�*B�B��B��B�5B�BB�GB�GB�UB�SB�HB�=B�*B�B��B��B��B��BпB��BεBβB̧BǇB�bB�JB�0B�7B�,B�$B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�;B��B��B��B	 �B	�B	�B	�B	�B	B	PB	eB	}B	�B	"�B	"�B	"�B	!�B	!�B	!�B	 �B	 �B	�B	�B	�B	#�B	(�B	+�B	0�B	1�B	4B	8#B	;5B	<<B	<<B	<;B	=BB	=BB	=BB	<<B	=AB	J�B	T�B	S�B	R�B	Q�B	Q�B	Y�B	d)B	iHB	lZB	m^B	nfB	qwB	v�B	x�B	z�B	}�B	��B	��B	��B	��B	�B	�B	�)B	�/B	�3B	�eB	��B	��B	��B	��B	��B	��B	��B	�B	�0B	�*B	�FB	�yB	˓B	͝B	ϪB	ЯB	ѸB	ѴB	ҼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ѸB	ϧB	͝B	̖B	аB	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�.B	�.B	�)B	�'B	�!B	�!B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�TB	�OB	�6B	�B	�B	�<B	�MB	�HB	�KB	�fB	�lB	�gB	�hB	�aB	�fB	�rB	�wB	�tB	�jB	�aB	�ZB	�TB	�NB	�NB	�SB	�XB	�WB	�ZB	�YB	�ZB	�WB	�SB	�OB	�MB	�RB	�VB	�ZB	�]B	�dB	�lB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�~B	�B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
	B

B
B
B
B
B
B
B
B
B
B
B
B
#B
"B
'B
%B
)B
&B
-B
,B
-B
2B
3B
4G�O�B
AB
"�B
(�B
-�B
1�B
5B
<,B
BSB
GsB
J�B
O�B
T�B
[�B
a
B
f)B
kGB
phB
tB
{�B
~�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.5 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451302016080714513020160807145130  AO  ARCAADJP                                                                    20160613091535    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160613091535  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160613091535  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145130  IP                  G�O�G�O�G�O�                