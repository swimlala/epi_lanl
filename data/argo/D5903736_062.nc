CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:39Z AOML 3.0 creation; 2016-05-31T19:14:34Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230539  20160531121435  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               >A   AO  4051_7090_062                   2C  D   APEX                            5368                            041511                          846 @��G��
1   @��Hl���@5$�/�e,j~��#1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    >A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dx��D�fD�@ D�� D���D�	�D��fD��fD��fD���D�33D��fD��fD�� D�<�D�vfD��D�3D�S3D�y�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dx��D�fD�@ D�� D���D�	�D��fD��fD��fD���D�33D��fD��fD�� D�<�D�vfD��D�3D�S3D�y�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A͝�A�ZA�v�A��TA��
A���A�ȴA˾wA˴9AˬA˧�A˝�AˑhA˅A�z�A�t�A�t�A�t�A�dZA�S�A�E�A�(�Aʩ�A�n�A�^5A�Q�A�&�A��yA��#A��A���A���A���AɅA�M�A�VA�x�A�A�~�A��A�Q�A��A�/A���A�XA�ZA���A��A��!A�^5A��A��PA���A�VA��A�ȴA��^A���A�n�A��uA��yA��-A�+A��+A�I�A��\A�VA��-A�-A���A�S�A���A��+A�9XA��A��yA�+A�G�A�hsA�JA�ĜA�^5A��A���A�n�A��A�jA��A��mA�t�A�%A��9A�;dA��
A�n�A�z�A�O�A���A��A�
=A�A��-A��7A�JA��A�dZA���A�Q�A���A��;A��RA��PA��yA��A��A���A���A?}A|{AxĜAw;dAv1'At�!As�hAqhsApVAnAljAj��Ahr�AfI�AeAd�Ad^5Ab�\A`��A^�RA]&�A[�;AZE�AV�uAT�\AS�PAS��AS��AS��AR��AR �AQƨAP��ANJAJv�AG7LAD�9ACƨABn�A?+A<9XA;�wA;�A:1'A9��A8��A6��A5C�A4�/A4Q�A3x�A1
=A/�PA.�A-�#A-%A,ĜA*��A*A)�;A(�yA$�A#`BA"1'A ȴA  �A��Ax�AO�AC�A
=A��A��A33A�RAE�A�TA7LAZA��A�FAdZA&�A�hAȴA+A�
A&�Ar�AC�A�\A�A�jAE�A��A
��A
ffA
bNA
bNA
^5A
bA	t�A��AM�A{A�A�-AO�A�#A�`AM�A1A�#Ap�A�A ��A �+A (�@�;d@�z�@�|�@�+@��@�7L@�~�@�&�@�S�@�h@�7L@�l�@�-@�bN@�|�@�dZ@�K�@�"�@�5?@�A�@ڏ\@؃@��y@Լj@�A�@�1@ӝ�@�v�@Ѳ-@љ�@с@�X@д9@�9X@��@�  @���@��;@Ͼw@ϝ�@υ@ύP@�33@́@�1'@�l�@�o@�v�@�J@�`B@���@�z�@Ǿw@�S�@���@�v�@ŉ7@�9X@�+@��@�p�@��`@���@���@��@�r�@�o@�V@��^@�X@���@��D@�Z@�1@�l�@��R@��+@�ff@�V@�%@��w@�|�@�S�@�33@�"�@��H@�5?@�7L@��D@�1'@��m@��H@�p�@���@�b@��F@��@�dZ@�33@���@��7@�?}@��/@���@��D@�z�@�z�@�r�@�r�@�bN@�9X@�  @��m@���@��w@���@�ȴ@�@�p�@�hs@�/@���@��D@�Z@�9X@��@���@�l�@���@�n�@�V@�M�@�E�@�=q@�5?@�5?@�V@�^5@�ff@�~�@�^5@�-@��@���@�hs@�&�@���@��@�Q�@���@��P@��@�v�@�ff@�5?@��@�@��#@�O�@�%@�bN@�A�@���@�dZ@�C�@�;d@�"�@��H@��H@�ȴ@�n�@���@�V@��`@��@�A�@��w@�|�@�"�@���@��+@��+@�v�@�v�@�n�@�J@��h@�O�@���@�Ĝ@�Ĝ@�Ĝ@��9@��@�I�@�1'@�I�@�z�@���@���@��D@�r�@�Z@���@�l�@���@�^5@�@��^@�G�@�%@�Ĝ@�r�@���@���@��@�l�@�;d@���@��@��+@�E�@�@��^@���@��7@�G�@��/@��@�z�@�Q�@�A�@�Q�@�Q�@�9X@�b@���@�;d@��@���@�=q@�@���@���@�hs@��@��/@��j@��u@�Z@�Q�@�9X@�  @��w@�M�@�=q@|I�@q&�@i7L@`b@X�@Qhs@J�!@F{@;�
@1��@,Z@)�#@$��@ ��@�@�#@�@�u@Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A͝�A�ZA�v�A��TA��
A���A�ȴA˾wA˴9AˬA˧�A˝�AˑhA˅A�z�A�t�A�t�A�t�A�dZA�S�A�E�A�(�Aʩ�A�n�A�^5A�Q�A�&�A��yA��#A��A���A���A���AɅA�M�A�VA�x�A�A�~�A��A�Q�A��A�/A���A�XA�ZA���A��A��!A�^5A��A��PA���A�VA��A�ȴA��^A���A�n�A��uA��yA��-A�+A��+A�I�A��\A�VA��-A�-A���A�S�A���A��+A�9XA��A��yA�+A�G�A�hsA�JA�ĜA�^5A��A���A�n�A��A�jA��A��mA�t�A�%A��9A�;dA��
A�n�A�z�A�O�A���A��A�
=A�A��-A��7A�JA��A�dZA���A�Q�A���A��;A��RA��PA��yA��A��A���A���A?}A|{AxĜAw;dAv1'At�!As�hAqhsApVAnAljAj��Ahr�AfI�AeAd�Ad^5Ab�\A`��A^�RA]&�A[�;AZE�AV�uAT�\AS�PAS��AS��AS��AR��AR �AQƨAP��ANJAJv�AG7LAD�9ACƨABn�A?+A<9XA;�wA;�A:1'A9��A8��A6��A5C�A4�/A4Q�A3x�A1
=A/�PA.�A-�#A-%A,ĜA*��A*A)�;A(�yA$�A#`BA"1'A ȴA  �A��Ax�AO�AC�A
=A��A��A33A�RAE�A�TA7LAZA��A�FAdZA&�A�hAȴA+A�
A&�Ar�AC�A�\A�A�jAE�A��A
��A
ffA
bNA
bNA
^5A
bA	t�A��AM�A{A�A�-AO�A�#A�`AM�A1A�#Ap�A�A ��A �+A (�@�;d@�z�@�|�@�+@��@�7L@�~�@�&�@�S�@�h@�7L@�l�@�-@�bN@�|�@�dZ@�K�@�"�@�5?@�A�@ڏ\@؃@��y@Լj@�A�@�1@ӝ�@�v�@Ѳ-@љ�@с@�X@д9@�9X@��@�  @���@��;@Ͼw@ϝ�@υ@ύP@�33@́@�1'@�l�@�o@�v�@�J@�`B@���@�z�@Ǿw@�S�@���@�v�@ŉ7@�9X@�+@��@�p�@��`@���@���@��@�r�@�o@�V@��^@�X@���@��D@�Z@�1@�l�@��R@��+@�ff@�V@�%@��w@�|�@�S�@�33@�"�@��H@�5?@�7L@��D@�1'@��m@��H@�p�@���@�b@��F@��@�dZ@�33@���@��7@�?}@��/@���@��D@�z�@�z�@�r�@�r�@�bN@�9X@�  @��m@���@��w@���@�ȴ@�@�p�@�hs@�/@���@��D@�Z@�9X@��@���@�l�@���@�n�@�V@�M�@�E�@�=q@�5?@�5?@�V@�^5@�ff@�~�@�^5@�-@��@���@�hs@�&�@���@��@�Q�@���@��P@��@�v�@�ff@�5?@��@�@��#@�O�@�%@�bN@�A�@���@�dZ@�C�@�;d@�"�@��H@��H@�ȴ@�n�@���@�V@��`@��@�A�@��w@�|�@�"�@���@��+@��+@�v�@�v�@�n�@�J@��h@�O�@���@�Ĝ@�Ĝ@�Ĝ@��9@��@�I�@�1'@�I�@�z�@���@���@��D@�r�@�Z@���@�l�@���@�^5@�@��^@�G�@�%@�Ĝ@�r�@���@���@��@�l�@�;d@���@��@��+@�E�@�@��^@���@��7@�G�@��/@��@�z�@�Q�@�A�@�Q�@�Q�@�9X@�b@���@�;d@��@���@�=q@�@���@���@�hs@��@��/@��j@��u@�Z@�Q�@�9X@�  @��w@�M�@�=q@|I�@q&�@i7L@`b@X�@Qhs@J�!@F{@;�
@1��@,Z@)�#@$��@ ��@�@�#@�@�u@Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�ZB�NB�BB�BB�BB�BB�HB�HB�HB�BB�HB�BB�HB�BB�BB�HB�NB�NB�TB�TB�ZB�TB�TB�TB�ZB�ZB�fB�mB�mB�mB�mB�fB�`B�ZB�HB�5B�B��B��B�}B��B�oB{�Bv�Bp�B_;BXBT�BQ�BN�BG�B>wB9XB6FB49B33B2-B0!B+B&�B$�B!�B�B�B{BbB%B��B�yB�B��BŢB�FB�'B�B�B��B�uB�1B�B}�Bw�Bm�BbNBW
B33B&�B!�B�B�BVB1B��B��B�B�/B��B�jB��Bo�B_;BZBW
BO�BI�BD�B<jB1'B#�B�B
�B
��B
�9B
�uB
x�B
r�B
^5B
E�B
9XB
1'B
(�B
"�B
�B
bB
B	��B	�B	�NB	�#B	��B	ƨB	�qB	�^B	�3B	��B	��B	�bB	�+B	~�B	s�B	aHB	[#B	\)B	cTB	gmB	ffB	e`B	dZB	dZB	`BB	T�B	E�B	7LB	,B	%�B	�B	
=B��B��B��B�B�B�B�TB�5B�)B�B��B��BǮBĜB��B�qB�dB�FB�3B�'B�B��B��B��B��B��B��B�{B�{B�{B�uB�hB�bB�VB�PB�JB�DB�7B�+B�B�B~�B|�Bz�Bw�Bu�Bs�Br�Bp�Bn�Bm�Bl�Bk�BjBiyBhsBhsBhsBhsBgmBffBffBe`Be`BdZBcTBbNB`BB^5B^5B^5B^5B]/B]/B\)B\)B[#BZBXBVBS�BW
BVBT�BVBVBVBXBW
BYBZBaHBbNBaHBaHB`BB`BBcTBdZBgmBhsBm�Bn�Bq�Bs�Bx�B|�B}�B� B�B�1B�PB�\B�bB�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�9B�?B�RB�wBÖBȴB��B��B��B��B��B�B�/B�;B�NB�TB�`B�mB�mB�sB�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	+B	1B	1B	JB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	'�B	(�B	)�B	)�B	)�B	+B	+B	+B	,B	/B	0!B	33B	49B	6FB	=qB	A�B	E�B	G�B	J�B	O�B	P�B	Q�B	Q�B	R�B	R�B	R�B	T�B	VB	XB	XB	XB	YB	ZB	[#B	\)B	\)B	]/B	_;B	_;B	`BB	bNB	aHB	cTB	jB	m�B	o�B	q�B	q�B	s�B	u�B	v�B	w�B	x�B	y�B	|�B	}�B	� B	�B	�%B	�=B	�JB	�\B	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�9B	�?B	�?B	�LB	�XB	�dB	�}B	ÖB	ŢB	ǮB	ȴB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
B
DB
�B
 �B
)�B
0!B
7LB
<jB
@�B
F�B
O�B
T�B
XB
]/B
aHB
dZB
gmB
k�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�cB�UB�JB�HB�MB�MB�QB�QB�PB�IB�PB�KB�OB�KB�MB�TB�VB�XB�]B�]B�_B�]B�\B�ZB�dB�aB�oB�uB�wB�wB�uB�lB�iB�bB�NB�<B�B��B��B��B��B�uB{�Bv�Bp�B_;BXBUBQ�BN�BG�B>zB9XB6MB4=B36B22B0$B+B&�B$�B!�B�B�B|BeB(B��B�xB�B��BŤB�IB�+B�B�B��B�vB�1B�B}�Bw�Bm�BbPBWB31B&�B!�B�B�BSB2B��B��B�B�-B��B�jB��Bo�B_:BZBWBO�BI�BD�B<nB1&B#�B�B
�B
��B
�<B
�zB
x�B
r�B
^=B
E�B
9_B
1+B
(�B
"�B
�B
lB
B	��B	�B	�VB	�-B	��B	ƱB	�zB	�hB	�<B	��B	��B	�pB	�8B	B	s�B	aSB	[0B	\5B	c`B	g{B	fsB	elB	dgB	ddB	`NB	UB	E�B	7ZB	,B	%�B	�B	
LB��B��B��B�B�B�B�fB�HB�<B�)B�
B��B��BıB��B��B�xB�YB�FB�>B�B��B��B��B��B��B��B��B��B��B��B�B�yB�jB�eB�]B�[B�KB�DB�.B�BB}Bz�Bw�Bu�Bs�Br�Bp�Bn�Bm�Bl�Bk�Bj�Bi�Bh�Bh�Bh�Bh�Bg�BfBf}BewBewBdrBckBbeB`YB^LB^KB^KB^JB]GB]FB\@B\@B[;BZ4BX)BVBTBW#BVBUBVBVBVBX$BW"BY-BZ3Ba_BbdBa^Ba^B`XB`YBcjBdmBg�Bh�Bm�Bn�Bq�Bs�Bx�B}B~B�B�#B�IB�gB�qB�wB�vB�{B��B��B��B��B��B��B��B��B��B��B�B�B�%B�4B�@B�DB�LB�SB�bB��BèB��B��B��B��B�B�B�"B�AB�LB�^B�dB�qB�}B�|B�B�B�B�B�B�B��B��B��B��B�B�B�B	 B	'B	:B	?B	?B	YB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	'�B	)B	*B	*
B	*
B	+B	+B	+B	,B	/)B	0-B	3@B	4FB	6TB	=~B	A�B	E�B	G�B	J�B	O�B	P�B	Q�B	Q�B	R�B	R�B	R�B	U	B	VB	XB	XB	XB	Y$B	Z)B	[0B	\5B	\5B	]9B	_FB	_HB	`NB	bZB	aSB	c^B	j�B	m�B	o�B	q�B	q�B	s�B	u�B	v�B	w�B	x�B	y�B	|�B	}�B	�
B	�B	�1B	�HB	�RB	�eB	�rB	�zB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�
B	�B	�#B	�*B	�/B	�;B	�AB	�BB	�HB	�IB	�SB	�`B	�nB	��B	ßB	ũB	ǸB	ȻB	ǸB	ǵB	��B	��B	��B	��B	��B	��B	� B	�B	�
B	�B	�B	�$B	�$B	�$B	�+B	�.B	�1B	�<B	�BB	�HB	�OB	�QB	�TB	�RB	�cB	�gB	�nB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
"B
KB
�B
 �B
*B
0'B
7RB
<nB
@�B
F�B
O�B
UB
XB
]2B
aLB
d^B
grB
k�B
q�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214352016053112143520160531121435  AO  ARCAADJP                                                                    20140721230539    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230539  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230539  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121435  IP                  G�O�G�O�G�O�                