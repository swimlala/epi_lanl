CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-16T17:21:51Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20230516172151  20230516172151  5906801 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      A   AO  9274                            2B  A   NAVIS_A                         1435                            170425                          863 @���'qu1   @����l& @<��E���dGE8�51   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A       @�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�	�D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��G@��GA�
A#�
AC�
Ac�
A��A��A��A��A��A��A�RA�RB ��B�]B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D�D�ǮD��D�G�DÇ�D�ǮD��D�G�Dć�D�ǮD��D�G�DŇ�D�ǮD��D�G�DƇ�D�ǮD��D�G�DǇ�D�ǮD��D�G�Dȇ�D�ǮD��D�G�Dɇ�D�ǮD��D�G�Dʇ�D�ǮD��D�G�Dˇ�D�ǮD��D�G�Ḋ�D�ǮD��D�G�D͇�D�ǮD��D�G�D·�D�ǮD��D�G�Dχ�D�ǮD��D�G�DЇ�D�ǮD��D�G�Dч�D�ǮD��D�G�D҇�D�ǮD��D�G�DӇ�D�ǮD��D�G�Dԇ�D�ǮD��D�G�DՇ�D�ǮD��D�G�Dև�D�ǮD��D�G�Dׇ�D�ǮD��D�G�D؇�D�ǮD��D�G�Dه�D�ǮD��D�G�Dڇ�D�ǮD��D�G�Dۇ�D�ǮD��D�G�D܇�D�ǮD��D�G�D݇�D�ǮD��D�G�Dއ�D�ǮD��D�G�D߇�D�ǮD��D�G�D���D�ǮD�
�D�G�DᇮD�ǮD��D�G�D⇮D�ǮD��D�G�D㇮D�ǮD��D�G�D䇮D�ǮD��D�G�D凮D�ǮD��D�G�D懮D�ǮD��D�G�D燮D�ǮD��D�G�D臮D�ǮD��D�G�D釮D�ǮD��D�G�DꇮD�ǮD��D�G�D뇮D�ǮD��D�G�D쇮D�ǮD��D�G�D퇮D�ǮD��D�G�DD�ǮD��D�G�DD�ǮD��D�G�D���D�ǮD��D�G�D�D�ǮD��D�G�D�D�ǮD��D�G�D�D�ǮD��D�G�D�D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D���D�HD�4{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AѴ9AѴ9AѴ9AѴ9AѸRAѺ^AѶFAѸRAѸRAѰ!Aѣ�AѓuA�  Aϩ�A΁A�v�A�{A��A���A��A�Q�A��A�(�A��A��
A��#A��7A��\A���A�  A�A�A���A��RA�p�A��A��\A��yA�1'A��A�A�O�A�=qA�Q�A��+A���A�I�A�1'A���A��
A�jA�{A���A�;dA�"�A��/A�E�A�33A�
=A�C�A��^A�dZA��A�oA�n�A�5?A��yA�ffA���A�"�A���A�{A�bA���A��A�^5A�ƨA�$�A��jA��+A�1A��RA��\A�A�(�A���A��RA�;dA���A�C�A��A���A��\A~  A{�Az1Aw�Av(�AuAt�yAtjAs\)Aq��Apn�An��Al$�Ak7LAj�RAiK�Ah�uAhbNAg�Ag;dAe�FAc��AbffAa��Aa�FAa�A`(�A]\)A[C�AZ$�AY�TAY�#AY�FAY�AY\)AVbAUAT^5AT9XAT�AS�#AS7LAR=qARAQx�APv�AN�AN1AM��AM&�AK�PAJ�!AI�;AIS�AH��AGƨAGS�AF��AF �AD�`AC��ABjAB{AB$�AA��AA�;AAƨAA\)A@�uA?�A?oA=��A="�A<�`A<VA;p�A:�\A:9XA9l�A7;dA4��A4�A4�uA4v�A4^5A4=qA4�A4  A3�A3��A3��A3A25?A1O�A/33A.M�A-x�A,~�A*�A*��A)�-A(��A(1'A'�PA&��A&�+A&�A%�;A%��A%l�A$ffA!�A ffA��A�+A�A?}A�AdZA��A��An�A �A�wAp�AA�AE�A�A��A�Av�AĜAx�AVAA|�A+A�HA�A;dA�7A
JA	l�A�+AC�A=qA��A7LAȴA��Ar�AffAZAM�A5?A|�@�ƨ@���@��@���@�;d@�`B@��D@�I�@���@�@��@��#@��@�1@�P@��@��@�?}@�D@���@��/@�1'@��
@�@�V@�E�@�-@��@�`B@�I�@�@��u@ް!@�V@ۥ�@�E�@�7L@�A�@�o@֟�@���@�V@ӕ�@Ұ!@љ�@��/@�9X@��@��;@�\)@���@�bN@ɑh@�r�@�1@Ǿw@�dZ@�K�@�
=@���@�J@��@� �@�{@�X@��j@��@��@��@�  @�S�@�o@���@���@��h@�r�@��@��+@���@�r�@��m@�;d@��\@�^5@��@�I�@�t�@��@���@�n�@�hs@��@�=q@�J@��#@�Ĝ@�r�@�Z@��@��@���@��@�bN@�|�@��H@���@��+@�5?@��@��@�%@���@� �@���@�dZ@��@���@���@�?}@��`@��@�Q�@�1@��P@�M�@�X@��/@��u@� �@�S�@�$�@��@�1@���@�|�@�K�@��H@���@��+@�V@�{@�@��@���@���@�bN@�Z@�A�@�b@��;@���@�K�@�o@��!@���@�/@�V@���@�Ĝ@��u@�I�@���@���@�J@��h@�hs@��@��@�z�@��
@��@�dZ@�;d@���@�E�@��@�G�@��`@�Z@��@��w@���@�\)@��y@���@��#@�O�@�%@��@�A�@��@�w@�P@K�@+@~�y@~��@~ff@~V@~E�@~E�@~{@}��@}V@|�j@|�D@|z�@|Z@|(�@|1@{ƨ@{C�@z�H@z�!@zn�@z=q@z�@y�7@y&�@xr�@x �@w�w@w;d@w+@v�+@v{@u�T@u��@u�@t�D@t�@s��@s"�@r�\@r�@q��@q&�@p��@pbN@o+@n@m`B@l��@l1@kC�@ko@ko@k@j�H@j��@jn�@j=q@i�@i�^@i��@ix�@iX@i7L@h�9@h �@g�w@g|�@gK�@g�@f�y@fȴ@f$�@f@e`B@d�j@d9X@d(�@d(�@d(�@cƨ@ct�@co@b�@b��@b~�@b^5@b=q@b-@a��@a�^@aX@a�@`�9@`r�@` �@`b@`A�@`�@`1'@_�;@_+@^v�@^5?@^@]�T@]��@\�@[�m@[�@[S�@Z�@Z��@Z~�@ZJ@Y��@YG�@Y%@X��@XQ�@Wl�@Wl�@W|�@W|�@W�P@W�@Wl�@W+@V5?@U@Up�@T�j@T�D@TZ@TZ@T(�@S��@St�@St�@St�@S�@St�@S"�@R�@R��@R�@QX@QG�@Q&�@P�`@P��@P�u@P�u@P�@Pr�@PQ�@P  @O��@O\)@O+@N�@N�R@N�+@Nff@NV@M�@Mp�@M?}@L�/@L�@Lj@LI�@L(�@K��@K�m@K�
@K�F@Kt�@KS�@K@J^5@I�#@Ihs@I7L@I&�@HĜ@HbN@G�@G�@G
=@F�@Fv�@F@E�T@E`B@D��@D�/@D��@D�@D�D@D1@C�@Co@B��@B~�@B-@A��@A�@A��@A%@@r�@?�;@?K�@>�y@>��@>�+@>V@>$�@=�T@=�@=V@<z�@<I�@<1@<1@<1@;ƨ@:M�@9��@9hs@9&�@8��@8�`@8��@8�9@8�@7��@7\)@7+@7
=@6��@6ȴ@65?@5/@4�@4��@4��@4j@3C�@2�H@2�@2��@2^5@2=q@2=q@2=q@2-@1�@1��@1G�@0��@0Ĝ@0Ĝ@0Ĝ@0��@0  @/�@/��@/\)@/
=@.ȴ@.�+@.$�@-��@-@-��@-�@-p�@-?}@-/@,�@,�j@,Z@,I�@,I�@,I�@,�@+�F@+S�@+"�@+"�@+o@*�H@*��@*n�@*-@)�@)��@)7L@(��@(�u@(b@'�@'�;@'��@'l�@&��@&��@&�y@&�R@&��@&��@&��@&�+@&ff@&$�@%�@%�T@%V@$j@$(�@#ƨ@#��@#S�@#o@"�!@"n�@"M�@"�@!�^@!��@!��@!�7@!G�@!7L@!&�@ ��@ ��@ Ĝ@ �9@ ��@ bN@  �@   @   @   @�@   @   @�@   @�@�;@�P@�@�+@E�@�@��@��@@��@�h@O�@?}@�@�@�/@j@ƨ@��@�@S�@"�@�@��@^5@=q@-@�#@�^@hs@�@��@�`@��@Ĝ@r�@�P@;d@
=@
=@�@��@{@��@V@��@��@I�@�@��@��@C�@o@�H@��@�@�@��@��@~�@n�@n�@M�@M�@=q@�@�@�@��@�@�#@��@��@x�@G�@�`@Ĝ@�9@�u@�@bN@�w@�P@K�@�@��@��@��@��@��@��@�+@�+@�+@ff@{@@�T@�-@�h@`B@/@��@��@�j@�D@Z@9X@(�@(�@�@1@�
@�F@��@�@dZ@"�@@
��@
��@
�\@
�\@
~�@
~�@
~�@
~�@
n�@	��@	�7@	G�@��@��@��@Q�@b@  @�;@��@�w@�w@�w@��@\)@;d@
=@�R@��@��@�+@5?@�@�T@��@@�-@��@�@�@�@`B@?}@/@��@�@�/@�j@�j@�j@�j@�@��@�D@j@I�@(�@�@��@�m@�
@ƨ@�F@��@�@t�@�@t�@o@�H@��@~�@^5@M�@=q@�@��@��@��@x�@X@�@ �`@ ��@ Ĝ@ �9@ �@ A�@ b@ b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AѴ9AѴ9AѴ9AѴ9AѸRAѺ^AѶFAѸRAѸRAѰ!Aѣ�AѓuA�  Aϩ�A΁A�v�A�{A��A���A��A�Q�A��A�(�A��A��
A��#A��7A��\A���A�  A�A�A���A��RA�p�A��A��\A��yA�1'A��A�A�O�A�=qA�Q�A��+A���A�I�A�1'A���A��
A�jA�{A���A�;dA�"�A��/A�E�A�33A�
=A�C�A��^A�dZA��A�oA�n�A�5?A��yA�ffA���A�"�A���A�{A�bA���A��A�^5A�ƨA�$�A��jA��+A�1A��RA��\A�A�(�A���A��RA�;dA���A�C�A��A���A��\A~  A{�Az1Aw�Av(�AuAt�yAtjAs\)Aq��Apn�An��Al$�Ak7LAj�RAiK�Ah�uAhbNAg�Ag;dAe�FAc��AbffAa��Aa�FAa�A`(�A]\)A[C�AZ$�AY�TAY�#AY�FAY�AY\)AVbAUAT^5AT9XAT�AS�#AS7LAR=qARAQx�APv�AN�AN1AM��AM&�AK�PAJ�!AI�;AIS�AH��AGƨAGS�AF��AF �AD�`AC��ABjAB{AB$�AA��AA�;AAƨAA\)A@�uA?�A?oA=��A="�A<�`A<VA;p�A:�\A:9XA9l�A7;dA4��A4�A4�uA4v�A4^5A4=qA4�A4  A3�A3��A3��A3A25?A1O�A/33A.M�A-x�A,~�A*�A*��A)�-A(��A(1'A'�PA&��A&�+A&�A%�;A%��A%l�A$ffA!�A ffA��A�+A�A?}A�AdZA��A��An�A �A�wAp�AA�AE�A�A��A�Av�AĜAx�AVAA|�A+A�HA�A;dA�7A
JA	l�A�+AC�A=qA��A7LAȴA��Ar�AffAZAM�A5?A|�@�ƨ@���@��@���@�;d@�`B@��D@�I�@���@�@��@��#@��@�1@�P@��@��@�?}@�D@���@��/@�1'@��
@�@�V@�E�@�-@��@�`B@�I�@�@��u@ް!@�V@ۥ�@�E�@�7L@�A�@�o@֟�@���@�V@ӕ�@Ұ!@љ�@��/@�9X@��@��;@�\)@���@�bN@ɑh@�r�@�1@Ǿw@�dZ@�K�@�
=@���@�J@��@� �@�{@�X@��j@��@��@��@�  @�S�@�o@���@���@��h@�r�@��@��+@���@�r�@��m@�;d@��\@�^5@��@�I�@�t�@��@���@�n�@�hs@��@�=q@�J@��#@�Ĝ@�r�@�Z@��@��@���@��@�bN@�|�@��H@���@��+@�5?@��@��@�%@���@� �@���@�dZ@��@���@���@�?}@��`@��@�Q�@�1@��P@�M�@�X@��/@��u@� �@�S�@�$�@��@�1@���@�|�@�K�@��H@���@��+@�V@�{@�@��@���@���@�bN@�Z@�A�@�b@��;@���@�K�@�o@��!@���@�/@�V@���@�Ĝ@��u@�I�@���@���@�J@��h@�hs@��@��@�z�@��
@��@�dZ@�;d@���@�E�@��@�G�@��`@�Z@��@��w@���@�\)@��y@���@��#@�O�@�%@��@�A�@��@�w@�P@K�@+@~�y@~��@~ff@~V@~E�@~E�@~{@}��@}V@|�j@|�D@|z�@|Z@|(�@|1@{ƨ@{C�@z�H@z�!@zn�@z=q@z�@y�7@y&�@xr�@x �@w�w@w;d@w+@v�+@v{@u�T@u��@u�@t�D@t�@s��@s"�@r�\@r�@q��@q&�@p��@pbN@o+@n@m`B@l��@l1@kC�@ko@ko@k@j�H@j��@jn�@j=q@i�@i�^@i��@ix�@iX@i7L@h�9@h �@g�w@g|�@gK�@g�@f�y@fȴ@f$�@f@e`B@d�j@d9X@d(�@d(�@d(�@cƨ@ct�@co@b�@b��@b~�@b^5@b=q@b-@a��@a�^@aX@a�@`�9@`r�@` �@`b@`A�@`�@`1'@_�;@_+@^v�@^5?@^@]�T@]��@\�@[�m@[�@[S�@Z�@Z��@Z~�@ZJ@Y��@YG�@Y%@X��@XQ�@Wl�@Wl�@W|�@W|�@W�P@W�@Wl�@W+@V5?@U@Up�@T�j@T�D@TZ@TZ@T(�@S��@St�@St�@St�@S�@St�@S"�@R�@R��@R�@QX@QG�@Q&�@P�`@P��@P�u@P�u@P�@Pr�@PQ�@P  @O��@O\)@O+@N�@N�R@N�+@Nff@NV@M�@Mp�@M?}@L�/@L�@Lj@LI�@L(�@K��@K�m@K�
@K�F@Kt�@KS�@K@J^5@I�#@Ihs@I7L@I&�@HĜ@HbN@G�@G�@G
=@F�@Fv�@F@E�T@E`B@D��@D�/@D��@D�@D�D@D1@C�@Co@B��@B~�@B-@A��@A�@A��@A%@@r�@?�;@?K�@>�y@>��@>�+@>V@>$�@=�T@=�@=V@<z�@<I�@<1@<1@<1@;ƨ@:M�@9��@9hs@9&�@8��@8�`@8��@8�9@8�@7��@7\)@7+@7
=@6��@6ȴ@65?@5/@4�@4��@4��@4j@3C�@2�H@2�@2��@2^5@2=q@2=q@2=q@2-@1�@1��@1G�@0��@0Ĝ@0Ĝ@0Ĝ@0��@0  @/�@/��@/\)@/
=@.ȴ@.�+@.$�@-��@-@-��@-�@-p�@-?}@-/@,�@,�j@,Z@,I�@,I�@,I�@,�@+�F@+S�@+"�@+"�@+o@*�H@*��@*n�@*-@)�@)��@)7L@(��@(�u@(b@'�@'�;@'��@'l�@&��@&��@&�y@&�R@&��@&��@&��@&�+@&ff@&$�@%�@%�T@%V@$j@$(�@#ƨ@#��@#S�@#o@"�!@"n�@"M�@"�@!�^@!��@!��@!�7@!G�@!7L@!&�@ ��@ ��@ Ĝ@ �9@ ��@ bN@  �@   @   @   @�@   @   @�@   @�@�;@�P@�@�+@E�@�@��@��@@��@�h@O�@?}@�@�@�/@j@ƨ@��@�@S�@"�@�@��@^5@=q@-@�#@�^@hs@�@��@�`@��@Ĝ@r�@�P@;d@
=@
=@�@��@{@��@V@��@��@I�@�@��@��@C�@o@�H@��@�@�@��@��@~�@n�@n�@M�@M�@=q@�@�@�@��@�@�#@��@��@x�@G�@�`@Ĝ@�9@�u@�@bN@�w@�P@K�@�@��@��@��@��@��@��@�+@�+@�+@ff@{@@�T@�-@�h@`B@/@��@��@�j@�D@Z@9X@(�@(�@�@1@�
@�F@��@�@dZ@"�@@
��@
��@
�\@
�\@
~�@
~�@
~�@
~�@
n�@	��@	�7@	G�@��@��@��@Q�@b@  @�;@��@�w@�w@�w@��@\)@;d@
=@�R@��@��@�+@5?@�@�T@��@@�-@��@�@�@�@`B@?}@/@��@�@�/@�j@�j@�j@�j@�@��@�D@j@I�@(�@�@��@�m@�
@ƨ@�F@��@�@t�@�@t�@o@�H@��@~�@^5@M�@=q@�@��@��@��@x�@X@�@ �`@ ��@ Ĝ@ �9@ �@ A�@ b@ b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BK�BK�BF�B9XB&�BB�PB5?B�B{BB  B%B�B2-B>wB>wBH�BM�BQ�Bn�Bx�By�B�B�7B�DB�1B�B~�B{�Bw�BhsB[#BK�B9XB�BJBB��B��B�B�yB�`B�B��B��B��B�wB�FB�B��B�\Bn�BcTB^5BXBN�BE�B9XB1'B&�B�BB
��B
��B
�B
�`B
�;B
�)B
�
B
��B
��B
ǮB
�jB
�B
��B
��B
�B
{�B
q�B
gmB
VB
E�B
1'B
(�B
�B
oB
\B
DB
+B
B	��B	�B	�ZB	�B	��B	��B	��B	��B	��B	ɺB	ǮB	ÖB	�qB	�3B	�!B	�B	�B	��B	�hB	�B	|�B	y�B	y�B	x�B	v�B	r�B	ffB	XB	R�B	O�B	O�B	L�B	K�B	F�B	D�B	B�B	?}B	9XB	49B	1'B	/B	(�B	"�B	 �B	�B	�B	�B	�B	�B	oB	bB	PB	B	B	%B	JB	
=B	1B	B	  B��B��B�B�B�yB�sB�`B�HB�;B�5B�#B��B��B��B��B��B��B��B��B��B��B��B��BɺBǮBÖB�wB�^B�LB�B��B��B��B��B��B��B��B��B��B�{B�hB�VB�7B�B}�Bx�Bv�Bt�Bq�Bn�Bk�BiyBiyBhsBgmBe`BdZBbNBaHB`BB_;B\)BXBS�BN�BF�BE�BC�BB�BA�B?}B>wB9XB5?B33B2-B.B,B+B&�B&�B%�B%�B%�B$�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBoBoBhBhBbBbB\B\B\B\BVBPBPBDBJBDB
=BDBDBDBDBJBDBDBJBJBPBPBVBVBVBPBPBJBbBhBhBhBhBoBhBoBhBuBuB{B�B�B�B�B�B�B�B�B �B!�B!�B"�B$�B%�B'�B)�B-B.B0!B1'B1'B1'B7LB9XB9XB9XB:^B<jBC�BF�BF�BF�BJ�BJ�BJ�BK�BN�BR�BT�BW
BZB]/B\)B]/B^5B_;B`BBbNBcTBdZBe`BgmBl�Bm�Bn�Bo�Bq�Br�Bs�Bt�Bv�B|�B�B�B�B�B�1B�PB�bB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�-B�9B�?B�LB�RB�^BBǮBȴBɺB��B��B��B��B�B�#B�/B�5B�BB�ZB�fB�yB�B�B�B�B�B��B��B��B	  B	B	B	B	+B	
=B	JB	oB	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	%�B	%�B	&�B	&�B	)�B	+B	-B	.B	/B	0!B	0!B	1'B	1'B	2-B	33B	49B	49B	6FB	6FB	7LB	:^B	;dB	?}B	@�B	B�B	D�B	D�B	F�B	H�B	I�B	I�B	J�B	M�B	O�B	P�B	Q�B	S�B	VB	VB	ZB	[#B	^5B	cTB	hsB	jB	m�B	p�B	s�B	t�B	t�B	t�B	u�B	v�B	v�B	w�B	y�B	z�B	z�B	{�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�1B	�7B	�DB	�PB	�VB	�VB	�VB	�hB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�9B	�LB	�^B	�qB	�}B	�}B	��B	��B	B	B	ÖB	ŢB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�BB	�HB	�HB	�HB	�NB	�NB	�ZB	�`B	�`B	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
	7B

=B
DB
DB
JB
JB
JB
PB
PB
VB
bB
bB
bB
bB
bB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BK�BK�BF�B9XB&�BB�PB5?B�B{BB  B%B�B2-B>wB>wBH�BM�BQ�Bn�Bx�By�B�B�7B�DB�1B�B~�B{�Bw�BhsB[#BK�B9XB�BJBB��B��B�B�yB�`B�B��B��B��B�wB�FB�B��B�\Bn�BcTB^5BXBN�BE�B9XB1'B&�B�BB
��B
��B
�B
�`B
�;B
�)B
�
B
��B
��B
ǮB
�jB
�B
��B
��B
�B
{�B
q�B
gmB
VB
E�B
1'B
(�B
�B
oB
\B
DB
+B
B	��B	�B	�ZB	�B	��B	��B	��B	��B	��B	ɺB	ǮB	ÖB	�qB	�3B	�!B	�B	�B	��B	�hB	�B	|�B	y�B	y�B	x�B	v�B	r�B	ffB	XB	R�B	O�B	O�B	L�B	K�B	F�B	D�B	B�B	?}B	9XB	49B	1'B	/B	(�B	"�B	 �B	�B	�B	�B	�B	�B	oB	bB	PB	B	B	%B	JB	
=B	1B	B	  B��B��B�B�B�yB�sB�`B�HB�;B�5B�#B��B��B��B��B��B��B��B��B��B��B��B��BɺBǮBÖB�wB�^B�LB�B��B��B��B��B��B��B��B��B��B�{B�hB�VB�7B�B}�Bx�Bv�Bt�Bq�Bn�Bk�BiyBiyBhsBgmBe`BdZBbNBaHB`BB_;B\)BXBS�BN�BF�BE�BC�BB�BA�B?}B>wB9XB5?B33B2-B.B,B+B&�B&�B%�B%�B%�B$�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBoBoBhBhBbBbB\B\B\B\BVBPBPBDBJBDB
=BDBDBDBDBJBDBDBJBJBPBPBVBVBVBPBPBJBbBhBhBhBhBoBhBoBhBuBuB{B�B�B�B�B�B�B�B�B �B!�B!�B"�B$�B%�B'�B)�B-B.B0!B1'B1'B1'B7LB9XB9XB9XB:^B<jBC�BF�BF�BF�BJ�BJ�BJ�BK�BN�BR�BT�BW
BZB]/B\)B]/B^5B_;B`BBbNBcTBdZBe`BgmBl�Bm�Bn�Bo�Bq�Br�Bs�Bt�Bv�B|�B�B�B�B�B�1B�PB�bB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�-B�9B�?B�LB�RB�^BBǮBȴBɺB��B��B��B��B�B�#B�/B�5B�BB�ZB�fB�yB�B�B�B�B�B��B��B��B	  B	B	B	B	+B	
=B	JB	oB	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	%�B	%�B	&�B	&�B	)�B	+B	-B	.B	/B	0!B	0!B	1'B	1'B	2-B	33B	49B	49B	6FB	6FB	7LB	:^B	;dB	?}B	@�B	B�B	D�B	D�B	F�B	H�B	I�B	I�B	J�B	M�B	O�B	P�B	Q�B	S�B	VB	VB	ZB	[#B	^5B	cTB	hsB	jB	m�B	p�B	s�B	t�B	t�B	t�B	u�B	v�B	v�B	w�B	y�B	z�B	z�B	{�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�1B	�7B	�DB	�PB	�VB	�VB	�VB	�hB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�9B	�LB	�^B	�qB	�}B	�}B	��B	��B	B	B	ÖB	ŢB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�BB	�HB	�HB	�HB	�NB	�NB	�ZB	�`B	�`B	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
	7B

=B
DB
DB
JB
JB
JB
PB
PB
VB
bB
bB
bB
bB
bB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�     ! $       ! : Q 3 ' $ "                                                                                    "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     > F00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230516172151                                          AO  ARCAADJP                                                                    20230516172151    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230516172151  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230516172151  QCF$                G�O�G�O�G�O�0               