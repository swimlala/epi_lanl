CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:33Z AOML 3.0 creation; 2016-08-07T21:51:14Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221433  20160807145114  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_030                   2C  D   APEX                            6529                            072314                          846 @�3���?�1   @�3�ff?�@1�|�hs�dI�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dys3D�fD�<�D�|�D�ɚD��D�I�D�� D��fD� D�0 D��fD��3D�3D�0 Dڐ D�ٚD�fD�9�D�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @@��@��@��A�\A&�\AF�\Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���C h�Ch�Ch�Ch�Ch�C
h�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C h�C"h�C$h�C&h�C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CT��CV��CXh�CZh�C\h�C^h�C`h�Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�'�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%=D%�=D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt��Dy�pD��D�I�D���D�ֹD�&�D�V�D��D�ÅD�D�=D���D��RD�RD�=DڝD��D��D�F�D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aʺ^AʼjAʼjAʺ^Aʺ^AʼjAʾwAʼjAʾwAʡ�AʅA�~�Aʣ�A�ĜAʋDA�K�A�9XA�-A�(�A�+A�+A��A�1A�&�A�C�A�K�A�K�A�O�A�XA�Q�A�S�A�O�A�VA���Aə�A�M�A�$�A�E�A�O�A�XA�bNA�O�A��A��AȓuA�z�A�1'A�VA��
A�ĜAǰ!AǕ�A�S�A�;dA���Aƙ�A�M�A�?}A�&�A��HA�A�A�v�A7A�K�A��7A�A�A�E�A�bA�5?A�A�A�C�A�?}A�33A���A�-A��A���A��FA�|�A���A�p�A��+A�p�A�|�A��A��!A�t�A���A���A�A�jA���A��mA�A��\A���A�1A��A���A���A�K�A��FA�"�A�p�A�n�A�ĜA�Q�A��A~�+A|ZA{p�Ay�;Au��At$�An5?Ae�FAb�A]��A[�AZ��AY�AU�;AR��AN��ALr�AKx�AI"�AGXAGoAF�/AFr�AE`BAD�uADJAB�AA�FA>I�A:ĜA8�DA5K�A3�TA3"�A2��A0�A.��A.v�A-XA,�A+�A+�A)�A(ȴA'�
A%�hA$�A#��A#�PA#&�A"��A"bNA!G�A�;A�
AM�A �A�`A��A�A�TAhsA
=AĜA��AȴA�!AbAC�A�DAVA9XA �AƨAt�A��A�AA�!A�A�A+A��A�!An�A��A�;A��AG�A
jA	��A	x�A	VA��A��A��A�HA��AA�A�^Ax�AO�A&�A�A�!AVA�A��A;dA��AM�A��At�A�@���@�n�@���@��@� �@��#@�33@�J@���@�X@���@�@�C�@�ȴ@�$�@��@�%@�u@�9X@���@�v�@�{@�h@�/@���@�u@�1'@�;d@���@��@�z�@��;@�;d@�+@�J@�-@�@���@�(�@㝲@��@��@�=q@�p�@���@�A�@߶F@�;d@�
=@��@��H@�E�@�V@ܴ9@�bN@�|�@�V@���@ى7@�O�@��/@؛�@�I�@���@׾w@��@�@�x�@ԋD@ӕ�@��H@�=q@Ѳ-@мj@Л�@Ь@��/@�?}@�&�@�Ĝ@�z�@�(�@�ƨ@Ͼw@϶F@ϕ�@υ@�t�@�S�@·+@Ͳ-@�p�@�O�@�%@�bN@��@˶F@˅@�S�@�
=@�ȴ@�~�@�^5@���@ə�@�O�@�&�@���@��`@���@ȣ�@�j@�Z@�Z@�Q�@��m@���@��@Ƨ�@Ƈ+@�V@��@�X@�z�@�(�@���@Å@�S�@�S�@��@�{@���@�`B@�7L@���@���@�
=@��R@��+@��@�@��h@��/@��@��F@��@���@��\@�ff@�J@��@��@�r�@�9X@��@�ƨ@�|�@�+@���@�ff@���@�?}@��@���@���@��w@�\)@���@�v�@�ff@�V@�E�@��@�J@��#@��@�7L@�I�@��@���@�ȴ@���@�n�@�-@�$�@��@��@�@���@�@��T@���@���@�&�@�r�@�ƨ@���@�dZ@�@�~�@�V@�E�@�=q@�=q@�-@�{@�@��^@�bN@���@��@��y@��R@�n�@�E�@�J@�`B@�%@���@��D@�(�@�b@��@�"�@�^5@�@��^@�7L@��u@�Z@�A�@�1@���@�
=@���@�5?@�@�`B@��9@�9X@�ƨ@�o@��\@�V@�{@�&�@�Ĝ@��9@��@�1@��;@��@�t�@�dZ@�;d@��R@�^5@���@���@��j@�r�@�1'@��w@�"�@��R@���@�~�@��@��/@�K�@}��@u`B@n�R@e`B@WK�@O+@H�`@D9X@=O�@7��@1��@+S�@&��@#o@ff@  @��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aʺ^AʼjAʼjAʺ^Aʺ^AʼjAʾwAʼjAʾwAʡ�AʅA�~�Aʣ�A�ĜAʋDA�K�A�9XA�-A�(�A�+A�+A��A�1A�&�A�C�A�K�A�K�A�O�A�XA�Q�A�S�A�O�A�VA���Aə�A�M�A�$�A�E�A�O�A�XA�bNA�O�A��A��AȓuA�z�A�1'A�VA��
A�ĜAǰ!AǕ�A�S�A�;dA���Aƙ�A�M�A�?}A�&�A��HA�A�A�v�A7A�K�A��7A�A�A�E�A�bA�5?A�A�A�C�A�?}A�33A���A�-A��A���A��FA�|�A���A�p�A��+A�p�A�|�A��A��!A�t�A���A���A�A�jA���A��mA�A��\A���A�1A��A���A���A�K�A��FA�"�A�p�A�n�A�ĜA�Q�A��A~�+A|ZA{p�Ay�;Au��At$�An5?Ae�FAb�A]��A[�AZ��AY�AU�;AR��AN��ALr�AKx�AI"�AGXAGoAF�/AFr�AE`BAD�uADJAB�AA�FA>I�A:ĜA8�DA5K�A3�TA3"�A2��A0�A.��A.v�A-XA,�A+�A+�A)�A(ȴA'�
A%�hA$�A#��A#�PA#&�A"��A"bNA!G�A�;A�
AM�A �A�`A��A�A�TAhsA
=AĜA��AȴA�!AbAC�A�DAVA9XA �AƨAt�A��A�AA�!A�A�A+A��A�!An�A��A�;A��AG�A
jA	��A	x�A	VA��A��A��A�HA��AA�A�^Ax�AO�A&�A�A�!AVA�A��A;dA��AM�A��At�A�@���@�n�@���@��@� �@��#@�33@�J@���@�X@���@�@�C�@�ȴ@�$�@��@�%@�u@�9X@���@�v�@�{@�h@�/@���@�u@�1'@�;d@���@��@�z�@��;@�;d@�+@�J@�-@�@���@�(�@㝲@��@��@�=q@�p�@���@�A�@߶F@�;d@�
=@��@��H@�E�@�V@ܴ9@�bN@�|�@�V@���@ى7@�O�@��/@؛�@�I�@���@׾w@��@�@�x�@ԋD@ӕ�@��H@�=q@Ѳ-@мj@Л�@Ь@��/@�?}@�&�@�Ĝ@�z�@�(�@�ƨ@Ͼw@϶F@ϕ�@υ@�t�@�S�@·+@Ͳ-@�p�@�O�@�%@�bN@��@˶F@˅@�S�@�
=@�ȴ@�~�@�^5@���@ə�@�O�@�&�@���@��`@���@ȣ�@�j@�Z@�Z@�Q�@��m@���@��@Ƨ�@Ƈ+@�V@��@�X@�z�@�(�@���@Å@�S�@�S�@��@�{@���@�`B@�7L@���@���@�
=@��R@��+@��@�@��h@��/@��@��F@��@���@��\@�ff@�J@��@��@�r�@�9X@��@�ƨ@�|�@�+@���@�ff@���@�?}@��@���@���@��w@�\)@���@�v�@�ff@�V@�E�@��@�J@��#@��@�7L@�I�@��@���@�ȴ@���@�n�@�-@�$�@��@��@�@���@�@��T@���@���@�&�@�r�@�ƨ@���@�dZ@�@�~�@�V@�E�@�=q@�=q@�-@�{@�@��^@�bN@���@��@��y@��R@�n�@�E�@�J@�`B@�%@���@��D@�(�@�b@��@�"�@�^5@�@��^@�7L@��u@�Z@�A�@�1@���@�
=@���@�5?@�@�`B@��9@�9X@�ƨ@�o@��\@�V@�{@�&�@�Ĝ@��9@��@�1@��;@��@�t�@�dZ@�;d@��R@�^5@���@���@��j@�r�@�1'@��w@�"�@��R@���G�O�@��@��/@�K�@}��@u`B@n�R@e`B@WK�@O+@H�`@D9X@=O�@7��@1��@+S�@&��@#o@ff@  @��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�\B�\B�\B�\B�\B�\B�\B�\B�oB�B�jB�;B��BbB�B�B�B�B�B�B�B'�B.BA�BS�BYBe`B�B��B�LB��B�B�mBB�B�B�B&�B)�B/B49B;dB?}BB�B?}BG�BL�BL�BJ�BL�BO�BR�BXBXB\)B[#B]/B^5B_;B^5B[#BW
BS�BR�BL�B8RB�B=qBL�BR�BVBVBVB_;Be`BZBS�BP�BM�BN�BD�B7LB.B$�B�BB�Bk�B#�BB��B�B�wB�B��B�By�BffBW
B?}B{B
�fB
ɺB
�qB
��B
�B
[#B
.B
B	�B	�B	�5B	ĜB	�3B	�7B	W
B	A�B	(�B	�B	�B	bB	B�B�B�B�mB�)B�B�
B�B��B��B��B��B��B��B��BǮBƨBǮBƨBƨBŢBƨBȴBǮBŢBĜBÖBƨBǮBǮBƨBƨBĜBǮBȴBɺB��B��BɺBB�XB�B��B�B�LB�?B�?B�9B�?B�FB�dBB��B�B�TB�B�B�B�B�B�B��B	  B	B	\B	�B	�B	�B	�B	�B	 �B	$�B	$�B	&�B	(�B	(�B	(�B	/B	/B	0!B	2-B	7LB	@�B	D�B	F�B	H�B	J�B	J�B	K�B	L�B	O�B	R�B	R�B	S�B	VB	W
B	ZB	ZB	YB	W
B	VB	XB	XB	YB	[#B	W
B	S�B	VB	VB	VB	XB	[#B	\)B	]/B	]/B	^5B	`BB	`BB	`BB	aHB	e`B	ffB	hsB	iyB	iyB	jB	iyB	k�B	n�B	p�B	q�B	t�B	v�B	y�B	}�B	~�B	� B	�B	� B	�B	�B	�B	�B	�+B	�DB	�PB	�VB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�FB	�FB	�?B	�9B	�-B	�-B	�3B	�9B	�?B	�RB	�dB	�qB	�wB	�}B	��B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�/B	�/B	�)B	�#B	�)B	�#B	�/B	�/B	�BB	�;B	�/B	�/B	�;B	�;B	�5B	�;B	�;B	�5B	�5B	�BB	�HB	�HB	�HB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�fB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B
	7B
DB
{B
�B
 �B
%�B
)�B
/B
<jB
D�B
G�B
N�B
T�B
[#B
^5B
cTB
e`B
iyB
m�B
p�B
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�7B�;B�;B�8B�:B�8B�:B�;B�MB��B�FB�B��B>BpBrBkBdBdBxB�B'�B-�BAgBS�BX�Be>B��B��B�+BͱB��B�NB�BaBbBnB&�B)�B.�B4B;CB?`BBpB?^BG�BL�BL�BJ�BL�BO�BR�BW�BW�B\B[B]B^B_B^B[BV�BS�BR�BL�B83B�B=SBL�BR�BU�BU�BU�B_Be>BY�BS�BP�BM�BN�BD{B7+B-�B$�BfB�B�ZBkaB#�B �B��B�_B�OB��B�]B��By�Bf=BV�B?VBXB
�>B
ɕB
�MB
��B
��B
[ B
-�B
�B	�B	�dB	�B	�{B	�B	�B	V�B	AmB	(�B	�B	B	EB	 �B�B�fB�oB�RB�B��B��B��B��B��B��B��B��B��B˫BǑBƌBǑBƋBƊBńBƎBȚBǑBŃBăB�zBƊBǐBǐBƍBƊBāBǏBȖBɝBʧB˪BɝB�sB�:B��B��B��B�/B�#B�$B�B�$B�)B�HB�oB̮B��B�6B�fB�rB�qB�xB��B�B��B��B	�B	8B	{B	�B	�B	�B	�B	 �B	$�B	$�B	&�B	(�B	(�B	(�B	.�B	.�B	/�B	2	B	7(B	@^B	DxB	F�B	H�B	J�B	J�B	K�B	L�B	O�B	R�B	R�B	S�B	U�B	V�B	Y�B	Y�B	X�B	V�B	U�B	W�B	W�B	X�B	Z�B	V�B	S�B	U�B	U�B	U�B	W�B	Z�B	\B	]B	]B	^B	`B	`B	`B	a!B	e9B	f?B	hMB	iOB	iPB	jWB	iRB	k^B	nrB	p}B	q�B	t�B	v�B	y�B	}�B	~�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	�'B	�+B	�;B	�=B	�EB	�LB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�	B	�B	�B	�'B	�:B	�FB	�LB	�TB	�_B	�vB	˙B	̠B	̡B	̢B	ͨB	ΰB	έB	ϴB	ϱB	ήB	άB	лB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�	B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�	B	�B	�B	�	B	�B	�B	�B	�B	�B	�6B	�9B	�AB	�@B	�AB	�DB	�@B	�>B	�@B	�BB	�AB	�?B	�>B	�?B	�>B	�9B	�@B	�?B	�@B	�BB	�GB	�XB	�_B	�]B	�^B	�fB	�fB	�cB	�cB	�dB	�dB	�cB	�dB	�\B	�cB	�cB	�cB	�bB	�dB	�cB	�fB	�aB	�cB	�dB	�eB	�eB	�dB	�dB	�fB	�cB	�iB	�jB	�rB	�pB	�oB	�oB	�vB	�uB	�yB	�vB	�yB	�wB	�sB	�wB	�qB	�B	�B	�B	�B	�{B	�|B	�|B	�{B	�|B	�B	�B	�B	�B	�B	�B	�B	�}B	�yB	�wB	�wB	�wB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
G�O�B
B
JB
zB
 �B
%�B
)�B
.�B
<9B
DjB
G�B
N�B
T�B
Z�B
^B
c"B
e/B
iHB
maB
puB
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.41 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451142016080714511420160807145114  AO  ARCAADJP                                                                    20150226221433    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221433  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221433  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145114  IP                  G�O�G�O�G�O�                