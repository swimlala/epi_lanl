CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-03T19:16:29Z AOML 3.0 creation; 2016-05-31T19:14:44Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150803191629  20160531121444  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               xA   AO  4051_7090_120                   2C  D   APEX                            5368                            041511                          846 @�d�S��1   @�d��� @31hr� ��d\Q��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    xA   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  BffB  B   B(  B0  B7��BB  BG33BO��BX  B`  Bh  BpffBxffB��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�3D���D�9�D��3D���D�3D�@ D���D��fD�� D��fD���D�� D���D�<�D�ffD�� D�3D�0 D�s3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  BffB  B   B(  B0  B7��BB  BG33BO��BX  B`  Bh  BpffBxffB��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�3D���D�9�D��3D���D�3D�@ D���D��fD�� D��fD���D�� D���D�<�D�ffD�� D�3D�0 D�s3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�t�A�r�A�n�A�p�A�t�A�v�A�x�A�t�A�x�A�bNA�ffA�S�A�9XA��A��HA���Aߧ�Aߩ�A�bNA�Q�A�G�A�A�1Aٛ�A�VA�r�A�$�A��A���A���AΛ�A�hsA�5?Aʴ9AʸRA�v�A�ȴA�1A��A�ƨAƁA��mAőhA��AA�p�A��A�r�A�
=A��A��A��HA���A��!A�VA�~�A��/A�$�A���A�ƨA�Q�A��9A�(�A�`BA��A��A�33A�=qA��7A�
=A���A�v�A�ƨA�XA�A�|�A��A���A���A��wA���A�bNA��
A���A�E�A�VA�XA�z�A�jA�t�A��A���A�JA��\A�&�A��A�\)A���A�  A��7A���A���A�\)A�-A�
=A�G�A�ĜA�33A��A�l�A�x�A���A���A��AC�Ay&�AsC�Am��Aj^5Af��Ac�mAcl�Aa��A`9XA[oAWƨAV��ATz�ARjAQhsAO��AL��AJ��AH �AFr�AD�RAB�A@�HA?��A>��A=p�A;��A9%A7p�A5S�A4^5A3dZA1��A09XA.bNA-C�A,A�A*�HA)33A'XA$�RA#/A"�9A!��A ĜAdZA�/A��AƨA�RA-AI�A�uA�AZA��A�AS�A�A�;A �A-A^5A��A"�A�TA$�A%A^5AK�A
�A5?A~�A7LAQ�A�#AG�A1A �9@�;d@�S�@�ƨ@��@��@��@���@�`B@�7L@��@�Ĝ@�j@�Q�@�1@�n�@�X@�Z@�t�@��@���@�&�@�Q�@���@�ȴ@�x�@���@�Ĝ@�Q�@�S�@�^@�r�@�C�@旍@䛦@�t�@�{@ߥ�@�%@ج@�C�@ղ-@�"�@У�@�K�@���@��@���@��@�^5@�@���@�1@��H@�J@ũ�@�O�@��;@�{@���@� �@��y@��@�^5@��@��^@�hs@�O�@���@��j@��@�Z@�1'@�  @���@�K�@��\@��7@��`@�j@���@��F@�t�@�;d@��^@��@�I�@�(�@�b@��w@���@�ȴ@�
=@�^5@�O�@��@��9@���@�j@��@���@�\)@��@���@��\@�ff@�J@��#@���@�p�@�O�@�7L@���@�r�@���@�dZ@�S�@���@��\@�{@�`B@�%@�Ĝ@��@���@���@�t�@�t�@�S�@�ȴ@�E�@�hs@���@�bN@�t�@��+@�J@��#@��^@���@��7@��@�G�@�%@���@��j@�j@�9X@�b@��;@��@���@���@��P@�|�@�t�@�l�@�S�@�33@��@��@���@�^5@���@��@���@��@�bN@�(�@��D@���@�bN@��@���@��m@��;@�t�@�+@��y@��@���@�V@���@��7@�G�@���@�Q�@�  @��@��w@��w@��@��@���@�K�@���@��H@���@��!@���@���@�@��H@��R@���@�~�@�ff@�5?@�&�@���@��`@���@���@�Ĝ@��D@�1'@�b@���@�@��y@��y@���@���@��@���@�J@��#@��^@���@��-@��7@�O�@�%@��/@���@�Ĝ@�Ĝ@��9@��u@��
@�l�@�+@�o@�
=@���@���@��@���@���@��\@�V@�@���@��T@��#@�@���@���@�hs@���@�Ĝ@��D@�9X@�b@��@�+@��y@��R@�n�@���@�hs@��7@��7@�?}@��@�bN@�(�@�  @��;@��F@��F@��F@��m@��
@��@�C�@�o@���@�J@��^@��h@�O�@�&�@�V@��`@��@yX@qX@j��@b�@[�@PA�@H  @@Q�@8b@1�7@-?}@(��@$9X@;d@��@ �@��@A�@@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�t�A�t�A�r�A�n�A�p�A�t�A�v�A�x�A�t�A�x�A�bNA�ffA�S�A�9XA��A��HA���Aߧ�Aߩ�A�bNA�Q�A�G�A�A�1Aٛ�A�VA�r�A�$�A��A���A���AΛ�A�hsA�5?Aʴ9AʸRA�v�A�ȴA�1A��A�ƨAƁA��mAőhA��AA�p�A��A�r�A�
=A��A��A��HA���A��!A�VA�~�A��/A�$�A���A�ƨA�Q�A��9A�(�A�`BA��A��A�33A�=qA��7A�
=A���A�v�A�ƨA�XA�A�|�A��A���A���A��wA���A�bNA��
A���A�E�A�VA�XA�z�A�jA�t�A��A���A�JA��\A�&�A��A�\)A���A�  A��7A���A���A�\)A�-A�
=A�G�A�ĜA�33A��A�l�A�x�A���A���A��AC�Ay&�AsC�Am��Aj^5Af��Ac�mAcl�Aa��A`9XA[oAWƨAV��ATz�ARjAQhsAO��AL��AJ��AH �AFr�AD�RAB�A@�HA?��A>��A=p�A;��A9%A7p�A5S�A4^5A3dZA1��A09XA.bNA-C�A,A�A*�HA)33A'XA$�RA#/A"�9A!��A ĜAdZA�/A��AƨA�RA-AI�A�uA�AZA��A�AS�A�A�;A �A-A^5A��A"�A�TA$�A%A^5AK�A
�A5?A~�A7LAQ�A�#AG�A1A �9@�;d@�S�@�ƨ@��@��@��@���@�`B@�7L@��@�Ĝ@�j@�Q�@�1@�n�@�X@�Z@�t�@��@���@�&�@�Q�@���@�ȴ@�x�@���@�Ĝ@�Q�@�S�@�^@�r�@�C�@旍@䛦@�t�@�{@ߥ�@�%@ج@�C�@ղ-@�"�@У�@�K�@���@��@���@��@�^5@�@���@�1@��H@�J@ũ�@�O�@��;@�{@���@� �@��y@��@�^5@��@��^@�hs@�O�@���@��j@��@�Z@�1'@�  @���@�K�@��\@��7@��`@�j@���@��F@�t�@�;d@��^@��@�I�@�(�@�b@��w@���@�ȴ@�
=@�^5@�O�@��@��9@���@�j@��@���@�\)@��@���@��\@�ff@�J@��#@���@�p�@�O�@�7L@���@�r�@���@�dZ@�S�@���@��\@�{@�`B@�%@�Ĝ@��@���@���@�t�@�t�@�S�@�ȴ@�E�@�hs@���@�bN@�t�@��+@�J@��#@��^@���@��7@��@�G�@�%@���@��j@�j@�9X@�b@��;@��@���@���@��P@�|�@�t�@�l�@�S�@�33@��@��@���@�^5@���@��@���@��@�bN@�(�@��D@���@�bN@��@���@��m@��;@�t�@�+@��y@��@���@�V@���@��7@�G�@���@�Q�@�  @��@��w@��w@��@��@���@�K�@���@��H@���@��!@���@���@�@��H@��R@���@�~�@�ff@�5?@�&�@���@��`@���@���@�Ĝ@��D@�1'@�b@���@�@��y@��y@���@���@��@���@�J@��#@��^@���@��-@��7@�O�@�%@��/@���@�Ĝ@�Ĝ@��9@��u@��
@�l�@�+@�o@�
=@���@���@��@���@���@��\@�V@�@���@��T@��#@�@���@���@�hs@���@�Ĝ@��D@�9X@�b@��@�+@��y@��R@�n�@���@�hs@��7@��7@�?}@��@�bN@�(�@�  @��;@��F@��F@��F@��m@��
@��@�C�@�o@���@�J@��^@��h@�O�@�&�@�V@��`@��@yX@qX@j��@b�@[�@PA�@H  @@Q�@8b@1�7@-?}@(��@$9X@;d@��@ �@��@A�@@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
��B
��B
=Bt�B�1B�B%�B_;Bt�B��B�RB��B�oB��B��BPB#�B5?BA�BXBXBS�BL�BT�B[#BS�BVBiyBm�Bm�BiyB\)BR�BK�BN�BQ�BO�BP�BQ�BR�BVBVBT�BR�BN�BL�BI�BG�B?}B+B�B\B	7BB��B��B��B��BB+B	7B%BB��B�B��B��B�B��BaHB�B��B�B��B��B�-B�B��B�BBŢB��B�{B|�BbNB<jB/B;dBXBB�B$�B  B
�B
�;B
�XB
}�B
[#B
!�B	�B	�FB	�{B	m�B	XB	[#B	O�B	B�B	#�B	uB	DB	B��B��B�B�fB�HB�/B�B��B��B��B��B��B��B��B��BȴBȴBǮBƨBĜBÖBÖBÖBB��B�jB�LB�B�B�B�B��B��B��B��B��B��B�B�RBÖB��B��B�
B�
B�B�#B��B��B�B��B�Bz�Bx�Bw�Bu�Br�Bq�Bp�Bq�Bs�Bu�Bx�Bx�Bx�B~�B� B�B�+B�bB�VB�JB�bB�hB�hB�hB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�?B�RB�dB�}BBBBǮB��B��B��B��B��B��B�
B�/B�BB�HB�ZB�fB�mB�yB�B�B�B�B��B��B��B	B	%B	1B		7B		7B	\B	oB	�B	�B	�B	�B	�B	"�B	$�B	%�B	%�B	&�B	'�B	(�B	)�B	.B	1'B	5?B	:^B	<jB	=qB	>wB	C�B	E�B	G�B	I�B	L�B	N�B	Q�B	T�B	[#B	]/B	]/B	_;B	aHB	bNB	e`B	iyB	iyB	jB	k�B	jB	k�B	l�B	m�B	n�B	o�B	o�B	o�B	o�B	q�B	t�B	v�B	w�B	x�B	y�B	y�B	y�B	{�B	|�B	}�B	~�B	�B	�B	�DB	�\B	�hB	�hB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�?B	�LB	�LB	�jB	�wB	�wB	�wB	��B	��B	ÖB	ĜB	ŢB	ŢB	ÖB	ÖB	ÖB	ĜB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�#B	�)B	�#B	�)B	�)B	�;B	�BB	�BB	�;B	�;B	�BB	�NB	�TB	�ZB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
	7B
DB
DB
DB
DB
JB
JB
PB
VB
VB
\B
\B
\B
hB
�B
�B
$�B
+B
1'B
8RB
A�B
G�B
M�B
S�B
YB
^5B
aHB
ffB
jB
m�B
q�B
t�B
x�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
��B
��B
>Bt�B�2B�B%�B_<Bt�B��B�TB��B�nB��B��BOB#�B5CBA�BXBXBS�BL�BT�B[&BS�BV	Bi~Bm�Bm�Bi|B\+BR�BK�BN�BQ�BO�BP�BQ�BR�BVBVBT�BR�BN�BL�BI�BG�B?�B+B�B^B	;BB��B��B��B��BB.B	9B%BB��B�B�B��B�B��BaLB�B��B�B��B��B�-B�B��B�BBŢB��B�}B|�BbOB<lB/B;eBXBB�B$�B
��B
�B
�?B
�XB
}�B
[&B
!�B	�B	�PB	��B	m�B	XB	[0B	O�B	B�B	#�B	�B	SB	B��B��B�B�wB�XB�CB�#B�B��B��B��B��B��B��B��B��B��B��BƼBįBëBéBëB¢B��B�{B�aB�.B�(B�"B�B�
B��B��B��B��B��B�B�dBèB��B�
B�B�B�2B�5B�B��B�B��B�*Bz�Bx�Bw�Bu�Br�Bq�Bp�Bq�Bs�Bu�Bx�Bx�Bx�BB�B� B�>B�wB�jB�]B�wB�~B�}B�B�xB�vB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�0B�?B�SB�fB�uB��B¡B¡B£BǾB��B��B��B��B�B�B�B�>B�RB�YB�iB�wB�|B�B�B�B�B��B��B��B�B	!B	3B	?B		GB		IB	lB	}B	�B	�B	�B	�B	�B	"�B	$�B	%�B	%�B	&�B	'�B	)B	*	B	.!B	12B	5MB	:kB	<vB	=~B	>�B	C�B	E�B	G�B	I�B	L�B	N�B	Q�B	UB	[/B	]:B	]:B	_JB	aTB	bXB	ejB	i�B	i�B	j�B	k�B	j�B	k�B	l�B	m�B	n�B	o�B	o�B	o�B	o�B	q�B	t�B	v�B	w�B	x�B	y�B	y�B	y�B	{�B	|�B	~B	B	�B	�(B	�LB	�fB	�rB	�tB	�tB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�BB	�FB	�WB	�YB	�sB	��B	��B	�B	��B	��B	ßB	ĥB	ŪB	ŮB	ÞB	ÞB	ÝB	ĥB	ưB	ưB	ȻB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�+B	�*B	�*B	�2B	�+B	�2B	�0B	�DB	�JB	�IB	�BB	�BB	�KB	�TB	�[B	�aB	�eB	�mB	�rB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B
 B
 B
 B
B
B
!B
,B
/B
+B
*B
*B
.B
+B
2B
	;B
JB
KB
KB
KB
PB
OB
VB
[B
_B
bB
bB
dB
mB
�B
�B
$�B
+B
1-B
8YB
A�B
G�B
M�B
S�B
YB
^7B
aMB
flB
j�B
m�B
q�B
t�B
x�B
}�B
�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214442016053112144420160531121444  AO  ARCAADJP                                                                    20150803191629    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150803191629  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150803191629  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121444  IP                  G�O�G�O�G�O�                