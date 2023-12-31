CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-04T10:15:52Z AOML 3.0 creation; 2016-06-01T00:08:23Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150204101552  20160531170823  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               iA   AO  4055_7112_105                   2C  D   APEX                            5374                            041511                          846 @�7ȸ�1   @�7�]��@:O�;dZ�dQ�$�/1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    iA   A   A   @�ff@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyy�D� D�C3D��3D�ٚD�3D�6fD�vfD���D�  D�C3D�vfD��fD�  D�VfD�|�D�3D�	�D�VfD�vfD�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A�\A((�AF�\Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���C h�Ch�Ch�Ch�Ch�C
h�Ch�C��Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C h�C"h�C$h�C&h�C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^h�C`h�Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%=D%�=D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX �DX��DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt�=Dy��D�D�PRD��RD��D�RD�C�D���D���D�D�PRD���D�ӅD�D�c�Dډ�D��RD��D�c�D�D�ֹ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hsA�^5A�ffA�^5A�dZA�dZA�dZA�`BA�\)A�`BA�l�A�bNA�ZA�VA���A���A���A���A�A�A�JA�VA�bA�bA�VA�JA�
=A�1A�%A�A�A�A�A���A��A��A��HA���A��wA��RA��-A��-A��A��A��A��!A��-A��-A��A���A���A���A���A���A��uA��uA��uA��hA��hA��hA��PA��+A�~�A�x�A�r�A�r�A�t�A�\)A�5?A�%A���A��A��#A���A���A�ƨA�A�A���A�x�A�G�A�(�A��A�1'A�bA�(�A��A�ȴA���A��DA��A�M�A�1'A��A�A�n�A�33A�
=A�;dA�A���A��/A��+A��A��A��uA�ZA���A�  A��;A��/A��wA��!A���A���A�A}?}A|ȴA{�#Az�!AzQ�Ay�7Aw�;Aw�Aup�Asp�Ar�DAq�AqS�Aq+Ap��Ao�PAol�Ao�7Ao
=Al��Aj��Aj~�Ajn�Ai��Ah��Ah�AhJAg�Af�Ac��Ab�uAaA_oA\5?AZ5?AX9XAV1'ATZAS`BAR�/AP�AOO�AN~�AM��AL�AJ{AI�
AI|�AH��AH�AH5?AHJAGl�AF��AFĜAFv�AF �AE�
AES�AD(�AB��A?�#A>��A=��A=
=A<VA;�wA:�jA8��A7�#A6�A5�TA5p�A5A4ĜA4jA3�wA2�yA1�mA/�A/\)A/
=A.��A.ffA,�DA,Q�A,{A+�#A*�A(��A&��A&r�A&9XA%��A%ƨA%��A%p�A%�A$�A#t�A"5?A =qAr�A�-A�jA�A��Ap�A��A��AAA��AhsA�FA �A�AdZA"�A�DA�;A�A�jA^5A��A
bNA��AO�A(�A&�A��AI�A�mA��A ��A ff@���@��D@��+@���@�\)@�X@�\)@�{@��@�p�@��@@�v�@���@ꟾ@�=q@��@�@�7@�O�@���@��#@�(�@�$�@��u@ߍP@ް!@�O�@���@�\)@�-@�r�@ղ-@�1'@��@̃@��
@˝�@���@ȼj@�{@�/@ă@�+@�~�@�x�@��@�{@�/@�9X@���@�C�@�n�@���@��@�1'@���@��@���@�S�@�p�@�V@�Ĝ@� �@�l�@�"�@�v�@�J@��@��#@���@���@�@���@��h@�?}@��D@��;@�S�@��@�~�@�x�@��u@���@�S�@�v�@���@���@��j@���@�+@��y@���@���@��R@���@��+@�$�@��@�%@�r�@��@���@���@���@�$�@��@�V@���@���@�Ĝ@�Z@��
@�5?@�9X@�t�@�K�@��y@�^5@�$�@��#@�`B@�Q�@��@�;d@��\@���@�hs@�&�@���@��@�(�@�  @���@��F@��@�~�@�E�@��@��@��@��-@��@��@�bN@�I�@�9X@� �@��F@�\)@�33@�ȴ@�~�@�^5@�M�@�M�@�5?@��@���@�x�@��@��D@�A�@�(�@��m@�|�@�33@�o@��@��+@�M�@�M�@�M�@�E�@�-@��@��-@�O�@��/@��D@�Z@�A�@�(�@�  @��;@���@�\)@�C�@�"�@���@���@���@��+@�v�@�=q@�{@��@���@���@��7@�p�@�`B@�X@�G�@��@�%@��/@��9@���@���@���@��@�I�@�(�@��@�  @�;@�w@l�@K�@~��@~��@~ff@~{@}��@}p�@}/@|�@|�/@|�@|�@|Z@{�m@{��@{C�@z��@y��@r�!@i�#@a&�@Y�7@S�F@N$�@JJ@Ax�@:�H@5�@.@*��@%�h@K�@��@�D@�w@
�\@|�@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�hsA�^5A�ffA�^5A�dZA�dZA�dZA�`BA�\)A�`BA�l�A�bNA�ZA�VA���A���A���A���A�A�A�JA�VA�bA�bA�VA�JA�
=A�1A�%A�A�A�A�A���A��A��A��HA���A��wA��RA��-A��-A��A��A��A��!A��-A��-A��A���A���A���A���A���A��uA��uA��uA��hA��hA��hA��PA��+A�~�A�x�A�r�A�r�A�t�A�\)A�5?A�%A���A��A��#A���A���A�ƨA�A�A���A�x�A�G�A�(�A��A�1'A�bA�(�A��A�ȴA���A��DA��A�M�A�1'A��A�A�n�A�33A�
=A�;dA�A���A��/A��+A��A��A��uA�ZA���A�  A��;A��/A��wA��!A���A���A�A}?}A|ȴA{�#Az�!AzQ�Ay�7Aw�;Aw�Aup�Asp�Ar�DAq�AqS�Aq+Ap��Ao�PAol�Ao�7Ao
=Al��Aj��Aj~�Ajn�Ai��Ah��Ah�AhJAg�Af�Ac��Ab�uAaA_oA\5?AZ5?AX9XAV1'ATZAS`BAR�/AP�AOO�AN~�AM��AL�AJ{AI�
AI|�AH��AH�AH5?AHJAGl�AF��AFĜAFv�AF �AE�
AES�AD(�AB��A?�#A>��A=��A=
=A<VA;�wA:�jA8��A7�#A6�A5�TA5p�A5A4ĜA4jA3�wA2�yA1�mA/�A/\)A/
=A.��A.ffA,�DA,Q�A,{A+�#A*�A(��A&��A&r�A&9XA%��A%ƨA%��A%p�A%�A$�A#t�A"5?A =qAr�A�-A�jA�A��Ap�A��A��AAA��AhsA�FA �A�AdZA"�A�DA�;A�A�jA^5A��A
bNA��AO�A(�A&�A��AI�A�mA��A ��A ff@���@��D@��+@���@�\)@�X@�\)@�{@��@�p�@��@@�v�@���@ꟾ@�=q@��@�@�7@�O�@���@��#@�(�@�$�@��u@ߍP@ް!@�O�@���@�\)@�-@�r�@ղ-@�1'@��@̃@��
@˝�@���@ȼj@�{@�/@ă@�+@�~�@�x�@��@�{@�/@�9X@���@�C�@�n�@���@��@�1'@���@��@���@�S�@�p�@�V@�Ĝ@� �@�l�@�"�@�v�@�J@��@��#@���@���@�@���@��h@�?}@��D@��;@�S�@��@�~�@�x�@��u@���@�S�@�v�@���@���@��j@���@�+@��y@���@���@��R@���@��+@�$�@��@�%@�r�@��@���@���@���@�$�@��@�V@���@���@�Ĝ@�Z@��
@�5?@�9X@�t�@�K�@��y@�^5@�$�@��#@�`B@�Q�@��@�;d@��\@���@�hs@�&�@���@��@�(�@�  @���@��F@��@�~�@�E�@��@��@��@��-@��@��@�bN@�I�@�9X@� �@��F@�\)@�33@�ȴ@�~�@�^5@�M�@�M�@�5?@��@���@�x�@��@��D@�A�@�(�@��m@�|�@�33@�o@��@��+@�M�@�M�@�M�@�E�@�-@��@��-@�O�@��/@��D@�Z@�A�@�(�@�  @��;@���@�\)@�C�@�"�@���@���@���@��+@�v�@�=q@�{@��@���@���@��7@�p�@�`B@�X@�G�@��@�%@��/@��9@���@���@���@��@�I�@�(�@��@�  @�;@�w@l�@K�@~��@~��@~ff@~{@}��@}p�@}/@|�@|�/@|�@|�@|Z@{�m@{��@{C�@z��@y��@r�!@i�#@a&�@Y�7@S�F@N$�@JJ@Ax�@:�H@5�@.@*��@%�h@K�@��@�D@�w@
�\@|�@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�mB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�fB�`B�`B�fB�mB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�yB�yB�yB�yB�yB�yB�yB�yB�yB�sB�mB�fB�fB�fB�fB�fB�fB�`B�ZB�ZB�TB�NB�NB�NB�;B�#B�
B��B��B��B��B��B��B��B��B��BɺBĜB��B�!B��B��B��B��B�1B�Be`BuB�mBȴB�uB\B�}B�DBP�BA�B=qB49B'�B�BuB	7B  B
��B
�B
��B
�-B
��B
��B
��B
��B
�hB
�B
q�B
m�B
ffB
_;B
\)B
T�B
L�B
H�B
:^B
,B
%�B
�B
�B
�B
�B
PB
JB
DB
%B	�B	�NB	�BB	�5B	�B	��B	��B	ƨB	�}B	�LB	��B	��B	�uB	�B	r�B	ffB	[#B	P�B	J�B	I�B	F�B	<jB	5?B	1'B	,B	$�B	�B	�B	�B	�B	�B	�B	�B	uB	hB	bB	VB	PB	
=B	+B	B��B�B�B�B�yB�fB�TB�;B�#B�
B��B��B��B��B��B��BȴBŢB��B�jB�^B�XB�RB�LB�RB�LB�LB�9B�!B�B��B��B��B��B��B��B��B��B��B��B�hB�7B�B}�B{�Bx�Bs�Bl�BffBdZBcTBbNBaHB^5BYBVBT�BS�BR�BP�BM�BH�BD�BB�B>wB9XB1'B,B)�B'�B&�B&�B%�B#�B!�B�B�B�B�B�B�B�B{B{B{BuBoBhBbBhBoBoBuBuBuBuBhBuBuBoBhBhBbB\B\BVBDBDB
=BVBPBVB\BVBPBPB\BbBbBoBoBoBoB�B�B�B�B�B�B�B�B�B�B �B"�B%�B,B-B.B/B1'B2-B49B5?B6FB6FB6FB6FB6FB6FB6FB7LB9XB;dB=qB>wB>wB?}BA�BF�BG�BJ�BN�BR�BS�B\)B^5B_;B`BB`BB`BB`BB`BBbNBcTBgmBjBl�Bm�Bp�Bq�Bt�Bv�Bx�Bx�Bx�By�B{�B}�B�B�bB��B��B��B��B��B��B��B��B��B�B�B�9B�LB�RB�^B�jB�wB�}B��B��BƨB��B��B��B��B��B��B�
B�B�#B�)B�)B�/B�BB�TB�ZB�sB�B�B�B�B�B�B�B�B��B��B��B	  B	B	+B	
=B	JB	PB	oB	{B	�B	�B	�B	�B	�B	�B	�B	%�B	-B	/B	1'B	33B	5?B	7LB	<jB	?}B	A�B	C�B	H�B	I�B	J�B	K�B	K�B	P�B	S�B	VB	XB	ZB	[#B	\)B	]/B	^5B	^5B	aHB	bNB	dZB	ffB	gmB	gmB	gmB	hsB	k�B	m�B	n�B	p�B	p�B	q�B	s�B	t�B	v�B	x�B	y�B	{�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	��B	�FB	��B	�HB	�B	��B

=B
oB
 �B
+B
5?B
>wB
D�B
K�B
R�B
YB
_;B
ffB
l�B
p�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�NB�FB�IB�HB�GB�IB�IB�HB�HB�FB�EB�IB�BB�CB�HB�LB�LB�TB�YB�YB�gB�mB�rB�uB�tB�tB�tB�qB�zB�wB�zB�wB�zB�tB�tB�sB�nB�gB�`B�bB�eB�eB�XB�[B�XB�\B�YB�YB�[B�[B�[B�XB�UB�LB�GB�GB�GB�GB�GB�GB�CB�:B�<B�6B�1B�1B�3B�B�	B��B��B��B��B��B��B��B��B��B͵BɚB�{B�hB�B��B��B��B�{B�B��Be@BRB�GBȌB�OB6B�SB�BP�BAdB=KB4B'�B�BOB	B
��B
��B
�{B
ϷB
�B
��B
��B
��B
��B
�BB
��B
q�B
mlB
f@B
_B
\B
T�B
L�B
H�B
:>B
+�B
%�B
�B
�B
�B
_B
+B
%B
!B
 B	�B	�-B	�!B	�B	��B	��B	̫B	ƆB	�]B	�+B	��B	��B	�TB	��B	r�B	fDB	[B	P�B	J�B	I�B	F�B	<LB	5"B	1B	+�B	$�B	�B	�B	�B	�B	nB	hB	eB	XB	KB	GB	9B	1B	
B	B	�B��B�B�|B�kB�^B�KB�:B�!B�B��B��B��BξBͶB̳BʦBȗBńB�gB�PB�DB�>B�7B�2B�6B�0B�1B�!B�B��B��B��B��B��B��B��B��B��B��B�tB�NB�B��B}�B{�Bx�Bs�BlsBfMBdABc:Bb6Ba/B^BX�BU�BT�BS�BR�BP�BM�BH�BD�BBvB>^B9&B1B+�B)�B'�B&�B&�B%�B#�B!�B�B�BvB�B}BoBMBbBHBGB]B<B4B2B5BXBWB\B]B^B[B5B^B[B;B4B4B/BABEB$BBB
	B#B8B#BCB#B3B6BCBHBGBWBTBWBSBKBeBrBvBwB�B�B�B�B�B �B"�B%�B+�B,�B-�B.�B1B2B4B5 B6*B6)B6)B6(B6(B6(B6)B7/B9;B;FB=TB>ZB>ZB?aBAkBF�BG�BJ�BN�BR�BS�B\B^B_B`"B`#B`!B`!B`"Bb.Bc4BgNBj_BliBmrBp�Bq�Bt�Bv�Bx�Bx�Bx�By�B{�B}�B��B�AB�]B�_B�lB�~B��B��B��B��B��B��B��B�B�)B�/B�;B�FB�QB�ZB�cB�cBƁBˡBͭBαBϺBϻB��B��B��B��B�B�B�B�B�.B�4B�MB�_B�dB�dB�dB�jB�jB�|B�B��B��B��B��B	�B	B	
B	#B	'B	FB	TB	VB	YB	VB	^B	qB	�B	�B	%�B	,�B	.�B	0�B	3
B	5B	7#B	<@B	?UB	A_B	CkB	H�B	I�B	J�B	K�B	K�B	P�B	S�B	U�B	W�B	Y�B	Z�B	[�B	]B	^B	^B	aB	b"B	d.B	f<B	g>B	g@B	g@B	hFB	k[B	meB	nmB	pxB	pxB	q|B	s�B	t�B	v�B	x�B	y�B	{�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�[B	�B	ʔB	�B	�uB	��B

B
=B
 �B
*�B
5B
>FB
DkB
K�B
R�B
X�B
_	B
f4B
lXB
ptB
r11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.41 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708232016053117082320160531170823  AO  ARCAADJP                                                                    20150204101552    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150204101552  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20150204101552  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170823  IP                  G�O�G�O�G�O�                