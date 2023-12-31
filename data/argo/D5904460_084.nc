CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-11T03:16:06Z AOML 3.0 creation; 2016-08-07T21:17:42Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151111031606  20160807141742  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               TA   AO  5285_8895_084                   2C  D   APEX                            6487                            072314                          846 @�}��/L1   @�}���<@-�z�G��c�$�/�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    TA   B   B   @�ff@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B'��B.  B8  B@  BG��BP  BXffBb  Bg33Bp  Bx  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy` D��D�@ D�s3D���D�3D�C3D�@ D���D� D�C3D�p Dǩ�D͓31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A�\A&�\AH(�Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B
=B!��B)=qB/��B9��BA��BI=qBQ��BZ
=Bc��Bh�
Bq��By��B���B���B�B���B���B���B���B���B���B���B���B���B�B�B�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C h�Ch�Ch�Ch�Ch�C
h�C��Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C O\C"h�C$h�C&h�C(h�C*h�C,h�C.h�C0h�C2��C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^h�C`h�Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D��D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%=D%�=D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5��D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt��Dyz=D��D�MD��RD���D� RD�PRD�MD���D�D�PRD�}DǶ�D͠R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aޛ�Aޛ�Aޙ�Aޗ�Aޛ�Aޛ�Aޟ�Aޛ�Aޝ�Aޝ�Aޛ�Aޙ�Aޛ�Aޗ�AދDA�~�A��#A��A��mA�C�A�r�A���A��uA�A�A�1A�/A�+A��TA�9XA��RA��
A��RA�|�A�\)Az$�Ay/Aw"�ArE�AmS�Aj�`Ai�^Ah��Ag�Ad$�A`I�AY/AU�ANr�AL1'AKG�AJ��AJE�AG�AF�/AFJADM�AB�!A>I�A:  A6��A4A�A0v�A.-A,ZA*��A(�A'�FA'/A&z�A& �A%p�A#�FA"-A!?}A bNAv�A?}A%A=qAv�A�^A��AAdZA�HA-A�A��A`BA��A=qA�^AO�AVAJA��At�A��A�yA��A�jAXA��AoA��A�#A�!A�7A
ȴAC�A
��A	��A��A�A��A��A��Ap�A\)A�hA��AS�A�/A��A��AȴAC�A�A��A��A�A�!AA�yA�#A�A�A"�A�hA�
A  Az�AI�AAO�A%A%A��Ar�AbNA9XA(�A��AC�A�AA �HA ��A Z@���@�|�@��@���@�n�@�M�@���@��T@��h@���@��@��D@�I�@�9X@�9X@��@��@��w@��H@�M�@�Q�@���@��+@�M�@��@��-@�K�@��@�C�@�{@�h@�O�@���@���@�G�@�Z@�9X@�j@�u@��;@�|�@��y@��@�1'@땁@��@�+@�&�@���@��@�~�@�@�V@���@�@��@��
@���@�O�@�9@�j@�  @�t�@�J@�9X@ڧ�@�@�ƨ@�V@�{@���@��#@��@�+@�ȴ@�v�@�=q@�5?@�-@�$�@��@��#@�Ĝ@�(�@ϕ�@�\)@�33@�
=@θR@�J@�?}@̬@̃@� �@˅@�33@ʏ\@��@ɉ7@�hs@�hs@�G�@�/@�V@�%@��@�Ĝ@ȃ@�1'@�ƨ@���@Ƨ�@Ə\@�~�@�=q@��T@�X@ċD@�(�@��@� �@��@���@å�@�\)@�;d@�+@��@�
=@��@¸R@�n�@��@���@�X@�V@���@��@��@�r�@�j@�Z@��@��@�;d@�ȴ@�^5@�M�@�J@��^@��h@�7L@�9X@��@���@��w@���@�t�@��@�v�@�$�@��@�V@�  @��@�t�@��y@�ff@�$�@��@���@���@��@�j@�A�@��;@�dZ@�"�@��H@���@�V@��@��#@��-@��7@�X@�/@��@�V@���@�I�@�dZ@��@�@���@�`B@�&�@��j@�Z@���@��
@�|�@��@�ȴ@��+@�-@���@���@���@��F@���@�;d@��R@�M�@���@���@�`B@��`@���@�Q�@�9X@�1'@�1@��F@��@��7@��j@�I�@��@�1@���@�;d@��@��H@���@�V@�@���@�`B@�&�@���@��u@��m@��P@�C�@���@�ȴ@�V@���@���@��7@�?}@��@��@��`@���@�z�@���@���@�C�@�o@�@��@�ȴ@���@�-@�J@���@��-@�/@���@��@��/@���@�A�@�(�@�(�@��@��m@���@�l�@�+@��@���@���@�v�@�{@���@���@��h@�?}@���@��u@�j@�1'@��@�b@���@��;@��
@��w@�t�@�K�@�+@��@�ȴ@���@��R@��+@�$�@��@���@���@�p�@�&�@��@���@���@���@��u@��D@�bN@��;@�l�@�dZ@�+@���@�=q@��@�o@�z�@w��@pQ�@g��@_\)@W\)@Q��@J=q@D��@=�@7|�@1G�@+�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  Aޛ�Aޛ�Aޙ�Aޗ�Aޛ�Aޛ�Aޟ�Aޛ�Aޝ�Aޝ�Aޛ�Aޙ�Aޛ�Aޗ�AދDA�~�A��#A��A��mA�C�A�r�A���A��uA�A�A�1A�/A�+A��TA�9XA��RA��
A��RA�|�A�\)Az$�Ay/Aw"�ArE�AmS�Aj�`Ai�^Ah��Ag�Ad$�A`I�AY/AU�ANr�AL1'AKG�AJ��AJE�AG�AF�/AFJADM�AB�!A>I�A:  A6��A4A�A0v�A.-A,ZA*��A(�A'�FA'/A&z�A& �A%p�A#�FA"-A!?}A bNAv�A?}A%A=qAv�A�^A��AAdZA�HA-A�A��A`BA��A=qA�^AO�AVAJA��At�A��A�yA��A�jAXA��AoA��A�#A�!A�7A
ȴAC�A
��A	��A��A�A��A��A��Ap�A\)A�hA��AS�A�/A��A��AȴAC�A�A��A��A�A�!AA�yA�#A�A�A"�A�hA�
A  Az�AI�AAO�A%A%A��Ar�AbNA9XA(�A��AC�A�AA �HA ��A Z@���@�|�@��@���@�n�@�M�@���@��T@��h@���@��@��D@�I�@�9X@�9X@��@��@��w@��H@�M�@�Q�@���@��+@�M�@��@��-@�K�@��@�C�@�{@�h@�O�@���@���@�G�@�Z@�9X@�j@�u@��;@�|�@��y@��@�1'@땁@��@�+@�&�@���@��@�~�@�@�V@���@�@��@��
@���@�O�@�9@�j@�  @�t�@�J@�9X@ڧ�@�@�ƨ@�V@�{@���@��#@��@�+@�ȴ@�v�@�=q@�5?@�-@�$�@��@��#@�Ĝ@�(�@ϕ�@�\)@�33@�
=@θR@�J@�?}@̬@̃@� �@˅@�33@ʏ\@��@ɉ7@�hs@�hs@�G�@�/@�V@�%@��@�Ĝ@ȃ@�1'@�ƨ@���@Ƨ�@Ə\@�~�@�=q@��T@�X@ċD@�(�@��@� �@��@���@å�@�\)@�;d@�+@��@�
=@��@¸R@�n�@��@���@�X@�V@���@��@��@�r�@�j@�Z@��@��@�;d@�ȴ@�^5@�M�@�J@��^@��h@�7L@�9X@��@���@��w@���@�t�@��@�v�@�$�@��@�V@�  @��@�t�@��y@�ff@�$�@��@���@���@��@�j@�A�@��;@�dZ@�"�@��H@���@�V@��@��#@��-@��7@�X@�/@��@�V@���@�I�@�dZ@��@�@���@�`B@�&�@��j@�Z@���@��
@�|�@��@�ȴ@��+@�-@���@���@���@��F@���@�;d@��R@�M�@���@���@�`B@��`@���@�Q�@�9X@�1'@�1@��F@��@��7@��j@�I�@��@�1@���@�;d@��@��H@���@�V@�@���@�`B@�&�@���@��u@��m@��P@�C�@���@�ȴ@�V@���@���@��7@�?}@��@��@��`@���@�z�@���@���@�C�@�o@�@��@�ȴ@���@�-@�J@���@��-@�/@���@��@��/@���@�A�@�(�@�(�@��@��m@���@�l�@�+@��@���@���@�v�@�{@���@���@��h@�?}@���@��u@�j@�1'@��@�b@���@��;@��
@��w@�t�@�K�@�+@��@�ȴ@���@��R@��+@�$�@��@���@���@�p�@�&�@��@���@���@���@��u@��D@�bN@��;@�l�@�dZ@�+@���@�=qG�O�@�o@�z�@w��@pQ�@g��@_\)@W\)@Q��@J=q@D��@=�@7|�@1G�@+�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB:^B:^B{�Bp�B{�B�Bs�B	T�B	`BB	_;B	o�B	o�B	m�B	gmB	aHB	\)B	XB	S�B	P�B	O�B	D�B	?}B	9XB	49B	2-B	/B	,B	)�B	%�B	"�B	&�B	33B	:^B	6FB	49B	33B	6FB	6FB	<jB	;dB	A�B	W
B	^5B	_;B	T�B	S�B	H�B	5?B	%�B	{B	+B��B	B	B	B	B��B��B��B��B��B�B�B�B�B�B�B��B��B	%B	\B	{B	uB	{B	{B	�B	�B	�B	�B	!�B	"�B	&�B	7LB	>wB	?}B	C�B	N�B	iyB	gmB	]/B	cTB	|�B	{�B	r�B	k�B	�B	�B	{�B	u�B	r�B	s�B	u�B	z�B	z�B	� B	�DB	��B	��B	��B	��B	��B	�B	�^B	ƨB	ȴB	ȴB	ƨB	ÖB	�}B	�^B	�FB	�?B	�?B	�LB	ȴB	��B	�B	�;B	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�TB	�5B	�B	�B	�B	�
B	�5B	�NB	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
\B
VB
bB
hB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
{B
{B
{B
uB
{B
{B
{B
{B
uB
{B
{B
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
'�B
'�B
(�B
(�B
)�B
+B
0!B
6FB
:^B
B�B
D�B
H�B
L�B
T�B
W
B
]/B
`BB
cTB
ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B;DB;FB;DB;GB;FB;DB;DB;DB;FB;DB;FB;DB;FB;GB:?B:>B{�Bp�B{�B�Bs�B	T�B	`,B	_'B	o�B	o�B	myB	gPB	a0B	\B	W�B	S�B	P�B	O�B	DB	?`B	9;B	4B	2B	.�B	+�B	)�B	%�B	"�B	&�B	3B	:@B	6'B	4B	3B	6'B	6&B	<KB	;GB	AiB	V�B	^B	_B	T�B	S�B	H�B	5B	%�B	[B	
B��B	 �B	�B	�B	�B��B��B��B��B��B�B�B�B�B�B�B��B��B	B	9B	YB	RB	YB	XB	cB	zB	}B	~B	!�B	"�B	&�B	7'B	>QB	?YB	CqB	N�B	iQB	gFB	]	B	c0B	|�B	{�B	r�B	k^B	��B	��B	{�B	u�B	r�B	s�B	u�B	z�B	z�B	�B	�B	�qB	�cB	�_B	��B	��B	��B	�6B	�}B	ȌB	ȈB	�}B	�jB	�TB	�5B	�B	�B	�B	�"B	ȊB	ΰB	��B	�B	�0B	�SB	�fB	�fB	�eB	�gB	�fB	�iB	�aB	�aB	�ZB	�_B	�YB	�ZB	�RB	�MB	�OB	�IB	�NB	�NB	�OB	�OB	�NB	�MB	�MB	�SB	�ZB	�eB	�fB	�qB	�yB	�zB	�zB	�~B	�zB	�~B	�B	�B	�xB	�qB	�lB	�`B	�RB	�)B	�B	��B	��B	��B	��B	�B	�B	�3B	�6B	�:B	�TB	�mB	�_B	�[B	�PB	�cB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
B
B
	B
		B
		B
	B
B
	
B
	B

B

B

B

B

B

B

B
B
B
B
B
B
B
!B
 B
!B
!B
(B
'B
(B
&B
-B
&B
4B
:B
6B
9B
8B
<B
@B
FB
FB
GB
MB
JB
LB
QB
LB
LB
LB
EB
JB
LB
KB
JB
FB
JB
LB
JB
KB
FB
MB
OB
WB
PB
QB
PB
PB
UB
VB
XB
XB
WB
^B
\B
aB
]B
dB
cB
dB
eB
bB
^B
dB
kB
jB
mB
nB
nB
nB
lB
pB
wB
qB
oB
xB
|B
{B
{B
|B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
'�B
'�B
(�B
(�G�O�B
*�B
/�B
6B
:.B
B_B
DkB
H�B
L�B
T�B
V�B
\�B
`B
c%B
f41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.41 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417422016080714174220160807141742  AO  ARCAADJP                                                                    20151111031606    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151111031606  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151111031606  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141742  IP                  G�O�G�O�G�O�                