CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:58Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125958  20190405100801  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��aL�h�1   @��a�>��@.nz�G��c�hr�!1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�ffB���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D��D�<�D�p D��3D���D�FfD���D��3D� D�C3D�y�D�ɚD�fD�S3Dڜ�D��3D��fD�  D�s3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A�\A&�\AF�\Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bj
=Bq��By��B���B���B���B���B���B���B�8RB���B���B���B���B���B���B�8RB���B���B���B���BȞ�B���B���B���B���B���B���B���B���B���B���B���B�B���C ��Ch�Ch�Ch�Ch�C
h�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C h�C"h�C$h�C&h�C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^h�C`h�Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%=D%�=D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm �Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt��Dy�=D��D�I�D�}D��RD���D�S�D���D��RD�D�PRD���D�ֹD�#�D�`RDک�D��RD��D�-D�RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�bA��A��A�$�A�$�A�&�A�&�A�(�A�(�A�+A�+A�+A�1'A�7LA�9XA�;dA�;dA�?}A�A�A�C�A�C�A�C�A�C�A�C�A�?}A�?}A�;dA�(�A�1A���A�p�A�Q�A�{Aɡ�A�t�A���Aȩ�Aȇ+A��A���A��Aá�A��#A�A�A��A�A�ZA�JA���A�dZA�bA�5?A�ĜA���A��A�O�A���A�VA��+A���A�ffA�z�A���A�7LA���A�t�A� �A���A��A���A�z�A�S�A�ȴA��jA�JA��`A���A�
=A��`A�x�A�C�A�ffA��jA� �A�A�=qA��A���A�JA�Q�A��A�=qA�;dA�G�A���A�=qA���A�{A��7A�Az�Aw;dAtM�AsS�ArI�AoƨAk�mAf�DAcC�Aa��A^-AZ^5AW�AV��AU�TAS��AQ�ANE�AL�AJE�AH��AFQ�AB�jA@A?K�A=��A<��A: �A8�`A7ƨA5��A5x�A5;dA4��A2�A/�^A-�A,��A+��A,�A+�PA*�A)�;A*�A+\)A,A,�+A,�A,bNA+�PA)��A'�^A'x�A'&�A&1A$n�A#�;A$��A$�/A$�A"�A!oA ��A �A z�A =qA�TA�A�jAE�A�A  A�uA�!A�^AM�A��A��A�PA�;A1A`BA��A�wA��A�7A�9AXA	�A	+A�#A��A��AC�A��A��A"�A��A�A�uAVAA�A$�A�;AƨA�A��A ��A �uA v�A E�A E�A =q@�v�@�/@��w@��@��@�r�@�Q�@�C�@�n�@�7L@�ƨ@��D@�  @�@�J@�V@�Ĝ@�t�@�5?@�%@��@�1'@�^5@��^@��@�@�X@��`@�r�@�o@�=q@�-@�h@��@畁@�K�@旍@�M�@�ff@��@�S�@���@���@�v�@�^@߾w@�dZ@�+@ޟ�@�M�@���@��@ܓu@�  @�t�@�K�@��@ڸR@�J@ى7@�p�@�p�@�X@�V@�V@�bN@׮@��y@�?}@�j@ӝ�@�K�@�C�@�o@�~�@ѡ�@���@�z�@�A�@Ϯ@���@͡�@�&�@�Ĝ@ʧ�@��y@��@�^5@��@�hs@�1@Ƈ+@�@�G�@��`@�A�@��;@��
@�C�@�@��@��@�M�@��@î@��y@���@��;@��@�dZ@���@���@� �@�Q�@���@���@���@���@�r�@��9@��@��u@��@��R@�~�@�ff@���@��@���@�dZ@�"�@��+@���@�X@�V@��u@��m@�33@��!@�M�@��T@�`B@�O�@��@��u@��P@�C�@�33@�ff@���@��7@�Ĝ@��D@�A�@�1'@�(�@��R@��@�@�{@�J@�$�@�E�@�@���@��7@�G�@�O�@�O�@�&�@��9@��@�  @��w@�C�@�ȴ@���@�{@��@���@�7L@��@���@���@�Z@�(�@�  @�ƨ@�\)@�@���@��\@��@��#@���@�`B@�&�@�&�@���@���@�I�@�  @��
@���@�|�@�\)@�
=@���@��R@�v�@�{@��T@���@�@��h@��@��D@�A�@��F@�+@���@��!@�~�@�5?@���@��h@�/@�Ĝ@���@�bN@��@�ƨ@��@�;d@��!@�-@�@��^@�x�@�O�@�&�@���@��`@��@�j@��@���@�l�@�K�@�K�@�33@��@���@�~�@�E�@�@�@�x�@�G�@���@��@�z�@�Q�@�b@��@�9X@��
@���@���@�S�@�
=@�V@�@���@�
=@�l�@��H@{o@q&�@g��@]`B@TI�@M�@C��@=V@65?@/;d@(��@!��@�j@�@1@b@��@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�bA��A��A�$�A�$�A�&�A�&�A�(�A�(�A�+A�+A�+A�1'A�7LA�9XA�;dA�;dA�?}A�A�A�C�A�C�A�C�A�C�A�C�A�?}A�?}A�;dA�(�A�1A���A�p�A�Q�A�{Aɡ�A�t�A���Aȩ�Aȇ+A��A���A��Aá�A��#A�A�A��A�A�ZA�JA���A�dZA�bA�5?A�ĜA���A��A�O�A���A�VA��+A���A�ffA�z�A���A�7LA���A�t�A� �A���A��A���A�z�A�S�A�ȴA��jA�JA��`A���A�
=A��`A�x�A�C�A�ffA��jA� �A�A�=qA��A���A�JA�Q�A��A�=qA�;dA�G�A���A�=qA���A�{A��7A�Az�Aw;dAtM�AsS�ArI�AoƨAk�mAf�DAcC�Aa��A^-AZ^5AW�AV��AU�TAS��AQ�ANE�AL�AJE�AH��AFQ�AB�jA@A?K�A=��A<��A: �A8�`A7ƨA5��A5x�A5;dA4��A2�A/�^A-�A,��A+��A,�A+�PA*�A)�;A*�A+\)A,A,�+A,�A,bNA+�PA)��A'�^A'x�A'&�A&1A$n�A#�;A$��A$�/A$�A"�A!oA ��A �A z�A =qA�TA�A�jAE�A�A  A�uA�!A�^AM�A��A��A�PA�;A1A`BA��A�wA��A�7A�9AXA	�A	+A�#A��A��AC�A��A��A"�A��A�A�uAVAA�A$�A�;AƨA�A��A ��A �uA v�A E�A E�A =q@�v�@�/@��w@��@��@�r�@�Q�@�C�@�n�@�7L@�ƨ@��D@�  @�@�J@�V@�Ĝ@�t�@�5?@�%@��@�1'@�^5@��^@��@�@�X@��`@�r�@�o@�=q@�-@�h@��@畁@�K�@旍@�M�@�ff@��@�S�@���@���@�v�@�^@߾w@�dZ@�+@ޟ�@�M�@���@��@ܓu@�  @�t�@�K�@��@ڸR@�J@ى7@�p�@�p�@�X@�V@�V@�bN@׮@��y@�?}@�j@ӝ�@�K�@�C�@�o@�~�@ѡ�@���@�z�@�A�@Ϯ@���@͡�@�&�@�Ĝ@ʧ�@��y@��@�^5@��@�hs@�1@Ƈ+@�@�G�@��`@�A�@��;@��
@�C�@�@��@��@�M�@��@î@��y@���@��;@��@�dZ@���@���@� �@�Q�@���@���@���@���@�r�@��9@��@��u@��@��R@�~�@�ff@���@��@���@�dZ@�"�@��+@���@�X@�V@��u@��m@�33@��!@�M�@��T@�`B@�O�@��@��u@��P@�C�@�33@�ff@���@��7@�Ĝ@��D@�A�@�1'@�(�@��R@��@�@�{@�J@�$�@�E�@�@���@��7@�G�@�O�@�O�@�&�@��9@��@�  @��w@�C�@�ȴ@���@�{@��@���@�7L@��@���@���@�Z@�(�@�  @�ƨ@�\)@�@���@��\@��@��#@���@�`B@�&�@�&�@���@���@�I�@�  @��
@���@�|�@�\)@�
=@���@��R@�v�@�{@��T@���@�@��h@��@��D@�A�@��F@�+@���@��!@�~�@�5?@���@��h@�/@�Ĝ@���@�bN@��@�ƨ@��@�;d@��!@�-@�@��^@�x�@�O�@�&�@���@��`@��@�j@��@���@�l�@�K�@�K�@�33@��@���@�~�@�E�@�@�@�x�@�G�@���@��@�z�@�Q�@�b@��@�9X@��
@���@���@�S�@�
=@�V@�@���@�
=@�l�@��H@{o@q&�@g��@]`B@TI�@M�@C��@=V@65?@/;d@(��@!��@�j@�@1@b@��@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�{B�{B�{B�{B�{B�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�^B�dB��B�B	uB	9XB	F�B	dZB	{�B	�JB	��B
,B
O�B
z�B
�jBB33BJ�BS�B_;B~�B��B��B��BBB
=BVBuB�B�B�B�B%�B6FB:^B9XB9XB7LB6FB2-B/B)�B!�B�BhB	7B��B�B�ZB�B��B�!B��B�B[#B=qBL�BF�B:^B2-B�B%B
�B
�/B
�^B
��B
� B
^5B
H�B
33B
�B
B	�B	�yB	�NB	�B	ƨB	��B	�=B	t�B	jB	[#B	H�B	>wB	7LB	0!B	#�B	�B	JB	B��B��B�B�NB�;B�5B�NB�HB�ZB�ZB�TB�B�B�B�B�B�NB�)B�#B�B��B	�B	+B	1'B	O�B	e`B	w�B	�bB	��B	��B	��B	�PB	�VB	��B	��B	��B	�\B	��B	�'B	�RB	�}B	�B	��B	��B	��B	�-B	�9B	�3B	��B	ȴB	��B	��B	��B	��B	�9B	�B	��B	�JB	{�B	q�B	�B	�PB	�7B	�+B	�B	~�B	|�B	y�B	t�B	o�B	k�B	iyB	iyB	jB	n�B	n�B	o�B	t�B	{�B	|�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�VB	��B	��B	��B	�{B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�LB	�RB	�qB	�qB	�dB	�RB	�LB	�LB	�FB	�9B	�9B	�3B	�^B	�^B	�RB	�FB	�9B	�?B	��B	�wB	�wB	�}B	�}B	��B	ĜB	ŢB	ƨB	ƨB	ŢB	ŢB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	ȴB	ŢB	ŢB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�TB	�B	�sB	�TB	�/B	�B	�;B	�fB	�fB	�B	�B	�B	�yB	�mB	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
1B
	7B

=B
DB
JB
JB
JB
JB
JB
JB
PB
PB
PB
VB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
oB
oB
hB
hB
bB
bB
bB
hB
hB
hB
hB
hB
bB
bB
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
oB
oB
oB
uB
{B
{B
{B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
"�B
#�B
'�B
2-B
8RB
=qB
C�B
I�B
K�B
R�B
XB
[#B
_;B
cTB
iyB
m�B
q�B
v�B
y�B
|�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�ZB�ZB�XB�YB�YB�YB�\B�[B�YB�`B�gB�`B�gB�yB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�=B�BB˧B�]B	RB	97B	F�B	d7B	{�B	�'B	��B
+�B
O�B
z�B
�JB�B3BJ�BS�B_B~�B��B�dB��B�B�B
B3BSBbBdBqB�B%�B6$B:9B94B96B7&B6"B2B.�B)�B!�BvBDB	B��B�uB�4B��B�dB��B��B��BZ�B=IBL�BF�B:7B2	B�B�B
�B
�B
�8B
��B
�B
^B
H�B
3B
|B
�B	�B	�NB	�%B	��B	�}B	��B	�B	t�B	jRB	Z�B	H�B	>KB	7!B	/�B	#�B	hB	B	�B��B��B�jB� B�B�B�B�B�+B�.B�'B�aB�wB�B�B�QB� B��B��B�B��B	vB	*�B	0�B	O�B	e2B	w�B	�5B	�oB	��B	�jB	� B	�%B	�]B	�XB	�WB	�+B	�]B	��B	�#B	�OB	��B	��B	��B	��B	��B	�
B	�B	�TB	ȂB	ΧB	̜B	ʏB	�RB	�
B	��B	��B	�B	{�B	qzB	��B	�B	�B	��B	��B	~�B	|�B	y�B	t�B	okB	kRB	iDB	iHB	jMB	neB	neB	olB	t�B	{�B	|�B	|�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�WB	�^B	�ZB	�JB	�4B	�UB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�>B	�@B	�1B	�!B	�B	�B	�B	�B	�B	�B	�+B	�+B	�B	�B	�B	�B	�PB	�EB	�BB	�JB	�IB	�UB	�iB	�lB	�sB	�uB	�lB	�lB	�sB	ȀB	ɅB	ɈB	ɅB	ʍB	ʍB	˔B	ʌB	̛B	˕B	ʎB	ȂB	�nB	�nB	�xB	ȀB	ȁB	ɈB	ɈB	̘B	ΥB	ΤB	ΥB	͟B	˓B	ɂB	˕B	͠B	̘B	ҾB	��B	��B	ѸB	ѷB	ΧB	ˑB	ʏB	ʏB	˒B	ˑB	͞B	ѷB	ЭB	��B	��B	��B	��B	�B	�UB	�AB	�B	��B	��B	�B	�3B	�4B	�JB	�\B	�UB	�BB	�:B	�2B	�3B	�QB	�hB	�mB	�fB	�bB	�eB	�hB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
	 B

B
B
B
B
B
B
B
B
B
B
B
 B
B
B
B
"B
"B
!B
"B
'B
(B
)B
%B
,B
,B
,B
.B
,B
,B
4B
4B
4B
5B
3B
2B
3B
3B
9B
8B
;B
;B
:B
;B
@B
8B
9B
4B
1B
,B
.B
.B
3B
2B
1B
2B
1B
-B
.B
&B
.B
-B
.B
4B
0B
2B
9B
;B
:B
AB
:B
:B
;B
AB
HB
DB
GB
IB
JB
FB
LB
KB
KB
LB
KB
PB
MB
OB
VB
WB
XB
WB
XB
XB
\B
[B
`B
zB
|B
�B
�B
 �B
 �B
!�B
!�B
"�B
#�B
'�B
1�B
8B
=:B
C`B
I�B
K�B
R�B
W�B
Z�B
_B
c!B
iDB
m[B
quB
v�B
y�B
|�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.41 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008012019040510080120190405100801  AO  ARCAADJP                                                                    20181121125958    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125958  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125958  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100801  IP                  G�O�G�O�G�O�                