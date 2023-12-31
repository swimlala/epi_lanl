CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  m   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-08T22:31:12Z creation      
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
resolution        =���   axis      Z        	�  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  E�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     	�  H    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  Q�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     	�  TD   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  ]�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  g�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  j   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  s�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  v@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  �<   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  ��   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �<   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �|   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  �|   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20230608223112  20230608223112  5904702 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      �A   AO  5449                            2B  A   NAVIS_A                         0470                            011514                          863 @ر��1   @رl�f@8O��-V�e�S���1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F       @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS�fDTfDT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DY��DZ� D[  D[� D\  D\� D\��D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dk��Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDtfDt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D��3D��3D�  D�@ D��3D��3D�  D�<�D�� D�� D�  D�@ D�� D�� D�3D�Vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @O\)@��@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC#�C=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt#�Cv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D�\D\D�\D \D �\D!\D!�\D"\D"��D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/�D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE��DF\DF�\DG\DG�\DH\DH�\DI�DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS�DS��DT�DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX��DY\DY�\DZ�DZ�\D[\D[�\D\\D\�\D]�D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df��Dg\Dg�\Dh\Dh�\Di�Di�\Dj\Dj�\Dk\Dk�\Dl�Dl��Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds��Dt�Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�J�D���D�ǮD��D�G�D���D�ǮD�
�D�G�D���D�ǮD�
�D�J�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD�{D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D�ǮD��D�G�D���D���D��D�G�D���D���D��D�G�D���D���D��D�D{D���D�ǮD��D�G�D���D�ǮD�
�D�^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A��A��FA��-A��hA��A�|�A�K�A��A�bA�%A���A��A��#A��!A��DA�-A��FA���A�n�A�A�A��A�VA�  A��A���A��7A��+A�~�A�Q�A�1'A�"�A�{A�VA�1A�A���A�+A��FA��hA��9A�M�A��mA���A���A��A�XA�(�A���A�|�A�bNA�`BA�VA�oA�\)A�(�A��wA���A��PA��+A�|�A�G�A�A�bA�\)A��A�{A��RA�hsA�?}A�=qA��\A��FA�ȴA��DA�33A��7A���A��TA��A�VA�?}A���A���A���A�"�A�dZA�1A��!A���A��RA�t�A�r�A��A��#A���A�jA�%A��7A���A�ƨA�&�A���A�"�A��A���A��A~ZA}&�Az��Ay�PAy"�Ax�+Av$�At�+AsArjAq�AoS�An��AnJAl��Al �Ak33Ah��Ag&�Af~�Aex�Ab�A_��A^��A]A[�AZE�AY��AYK�AXĜAX�+AW�ATA�AS;dAP�`AO%AM�mAM�AMO�AM&�AM%AL�HAL��AL��AL�RAL��AL�+AK��AH�!AF�HAF��AFv�AFbAE�
AE��AD�!AB�\AA��A@n�A?�mA?&�A>JA=K�A<-A:�A9��A9hsA9C�A9�A8��A7��A7A6�uA5�A3�A0��A/
=A-ƨA,�jA+��A*��A*ZA)hsA'��A&�/A%�A$��A#�A"JA�wA�TA%A�DA�^Az�A�A%AS�A�DA$�A�A�wA|�A�A�9A��AJA	��A�/AbAhsA/A��A�/A��A�!A��A�DA~�AQ�AVA9XA1'AJA�TA�hAoA�A&�A(�A��AXA �9A A�@���@�S�@��@�=q@�V@�\)@���@�X@�;d@�$�@��#@���@���@���@�\@��@�@�  @�;d@@�@��@��/@㕁@��@���@◍@��#@�h@��@��@�"�@�^5@�J@��@��@ݲ-@ݙ�@݉7@�x�@�x�@�p�@�?}@��@ܣ�@ܛ�@�9X@��@ׅ@ҧ�@�ȴ@�=q@͙�@�O�@��@�%@��`@̬@̣�@�r�@�j@�bN@�(�@�  @��;@˅@�C�@��@ʟ�@�v�@���@ȋD@Ƨ�@Ų-@��@ě�@�9X@þw@�S�@¸R@�^5@�{@��#@���@�%@��P@���@�E�@�5?@���@��T@��-@�7L@��j@���@�S�@�"�@���@�{@��9@���@��+@��@���@��@�r�@�A�@��
@�S�@�~�@���@�7L@�%@���@��@�hs@��@�j@���@���@��9@�9X@� �@�  @�  @���@�33@�5?@���@�  @��+@�X@��u@�x�@�1'@�M�@��P@�@��R@��!@���@���@��\@��\@��+@��+@�n�@�$�@��@�@���@��h@�p�@�O�@�G�@�/@���@���@�Ĝ@�r�@�1@�ƨ@��P@�+@���@���@��+@�^5@�@���@��7@���@��/@��9@��D@�I�@�(�@�b@�1@��m@��w@��P@�S�@�;d@�+@�"�@��@�o@��@��+@�{@��@��#@�?}@��@�j@��@�@��@��m@�C�@��@�o@���@��H@�ȴ@��!@���@��\@�v�@�n�@�^5@�$�@�{@�J@�@��@���@���@���@���@�x�@��`@�r�@��@~�y@}�@}p�@}O�@|�@{�F@{dZ@{S�@{"�@z~�@y�@y�@yx�@xr�@wl�@wK�@v�R@uO�@t��@t��@t�D@tZ@tj@s�F@rn�@q��@q�#@q�#@q�#@q��@qx�@p��@pQ�@pA�@pb@o�;@o��@o�P@oK�@n��@n��@nE�@n$�@n{@n{@m��@m�@mO�@m/@l�j@k�@j�H@j-@i��@i�7@i7L@hĜ@g
=@e�@ep�@d�@d��@d�@c�F@c��@cdZ@c33@c33@cC�@c33@c"�@b��@bM�@a&�@`�`@`�`@`�`@`Ĝ@`Ĝ@`Ĝ@`��@`�@`  @_l�@^��@\�@[�m@[S�@ZM�@Z=q@ZJ@Y�@Yx�@YX@X��@X��@X1'@WK�@WK�@W;d@V��@Vȴ@V��@VE�@VV@VV@VE�@VE�@V5?@V{@V5?@VV@U�@V@V@U`B@S�m@Q�7@Q�^@Q�#@RM�@R��@Rn�@Q��@R�@Q��@Q��@Q7L@P��@Q��@Q�^@Rn�@R~�@Q��@O�@N�R@PA�@P1'@Mp�@M/@L�@L�D@L�D@J�@Ihs@HQ�@G|�@Fȴ@E�@B�@=/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A��A��FA��-A��hA��A�|�A�K�A��A�bA�%A���A��A��#A��!A��DA�-A��FA���A�n�A�A�A��A�VA�  A��A���A��7A��+A�~�A�Q�A�1'A�"�A�{A�VA�1A�A���A�+A��FA��hA��9A�M�A��mA���A���A��A�XA�(�A���A�|�A�bNA�`BA�VA�oA�\)A�(�A��wA���A��PA��+A�|�A�G�A�A�bA�\)A��A�{A��RA�hsA�?}A�=qA��\A��FA�ȴA��DA�33A��7A���A��TA��A�VA�?}A���A���A���A�"�A�dZA�1A��!A���A��RA�t�A�r�A��A��#A���A�jA�%A��7A���A�ƨA�&�A���A�"�A��A���A��A~ZA}&�Az��Ay�PAy"�Ax�+Av$�At�+AsArjAq�AoS�An��AnJAl��Al �Ak33Ah��Ag&�Af~�Aex�Ab�A_��A^��A]A[�AZE�AY��AYK�AXĜAX�+AW�ATA�AS;dAP�`AO%AM�mAM�AMO�AM&�AM%AL�HAL��AL��AL�RAL��AL�+AK��AH�!AF�HAF��AFv�AFbAE�
AE��AD�!AB�\AA��A@n�A?�mA?&�A>JA=K�A<-A:�A9��A9hsA9C�A9�A8��A7��A7A6�uA5�A3�A0��A/
=A-ƨA,�jA+��A*��A*ZA)hsA'��A&�/A%�A$��A#�A"JA�wA�TA%A�DA�^Az�A�A%AS�A�DA$�A�A�wA|�A�A�9A��AJA	��A�/AbAhsA/A��A�/A��A�!A��A�DA~�AQ�AVA9XA1'AJA�TA�hAoA�A&�A(�A��AXA �9A A�@���@�S�@��@�=q@�V@�\)@���@�X@�;d@�$�@��#@���@���@���@�\@��@�@�  @�;d@@�@��@��/@㕁@��@���@◍@��#@�h@��@��@�"�@�^5@�J@��@��@ݲ-@ݙ�@݉7@�x�@�x�@�p�@�?}@��@ܣ�@ܛ�@�9X@��@ׅ@ҧ�@�ȴ@�=q@͙�@�O�@��@�%@��`@̬@̣�@�r�@�j@�bN@�(�@�  @��;@˅@�C�@��@ʟ�@�v�@���@ȋD@Ƨ�@Ų-@��@ě�@�9X@þw@�S�@¸R@�^5@�{@��#@���@�%@��P@���@�E�@�5?@���@��T@��-@�7L@��j@���@�S�@�"�@���@�{@��9@���@��+@��@���@��@�r�@�A�@��
@�S�@�~�@���@�7L@�%@���@��@�hs@��@�j@���@���@��9@�9X@� �@�  @�  @���@�33@�5?@���@�  @��+@�X@��u@�x�@�1'@�M�@��P@�@��R@��!@���@���@��\@��\@��+@��+@�n�@�$�@��@�@���@��h@�p�@�O�@�G�@�/@���@���@�Ĝ@�r�@�1@�ƨ@��P@�+@���@���@��+@�^5@�@���@��7@���@��/@��9@��D@�I�@�(�@�b@�1@��m@��w@��P@�S�@�;d@�+@�"�@��@�o@��@��+@�{@��@��#@�?}@��@�j@��@�@��@��m@�C�@��@�o@���@��H@�ȴ@��!@���@��\@�v�@�n�@�^5@�$�@�{@�J@�@��@���@���@���@���@�x�@��`@�r�@��@~�y@}�@}p�@}O�@|�@{�F@{dZ@{S�@{"�@z~�@y�@y�@yx�@xr�@wl�@wK�@v�R@uO�@t��@t��@t�D@tZ@tj@s�F@rn�@q��@q�#@q�#@q�#@q��@qx�@p��@pQ�@pA�@pb@o�;@o��@o�P@oK�@n��@n��@nE�@n$�@n{@n{@m��@m�@mO�@m/@l�j@k�@j�H@j-@i��@i�7@i7L@hĜ@g
=@e�@ep�@d�@d��@d�@c�F@c��@cdZ@c33@c33@cC�@c33@c"�@b��@bM�@a&�@`�`@`�`@`�`@`Ĝ@`Ĝ@`Ĝ@`��@`�@`  @_l�@^��@\�@[�m@[S�@ZM�@Z=q@ZJ@Y�@Yx�@YX@X��@X��@X1'@WK�@WK�@W;d@V��@Vȴ@V��@VE�@VV@VV@VE�@VE�@V5?@V{@V5?@VV@U�@V@V@U`B@S�m@Q�7@Q�^@Q�#@RM�@R��@Rn�@Q��@R�@Q��@Q��@Q7L@P��@Q��@Q�^@Rn�@R~�@Q��@O�@N�R@PA�@P1'@Mp�@M/@L�@L�D@L�D@J�@Ihs@HQ�@G|�@Fȴ@E�@B�@=/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BG�BG�BG�BG�BG�BH�BK�BK�BK�BJ�BJ�BJ�BJ�BK�BK�BK�BJ�BK�BL�BM�BM�BQ�BR�BR�BS�BVBW
BW
BW
BVBXBYBZB]/B]/B^5B_;B_;B_;B_;B]/BYBW
B^5BffBjBn�Bn�Bo�Bs�Bw�B~�B� B�B�B�B�B�B�B�B�B�JB�PB�PB�DB��B��B��B��B�1Bx�By�Bu�BQ�B49B&�B
=B��B�B�yB��B�jB�3B��B�oB�=B�Bm�B\)BJ�BA�B<jB5?B(�B�B��B�B�B�mB�TB�/B��B�jB��B��B��B�\B�7B�Bo�BI�B:^B0!B!�B�B�BhBB
��B
�B
�sB
�HB
�B
��B
��B
ĜB
�}B
�RB
�B
��B
��B
��B
�B
v�B
r�B
k�B
aHB
XB
R�B
P�B
L�B
I�B
C�B
33B
,B
!�B
�B
uB
oB
hB
bB
\B
VB
VB
PB
PB
JB

=B
B	��B	�B	�B	�B	�B	�sB	�`B	�;B	��B	��B	��B	ǮB	ÖB	�}B	�dB	�FB	�'B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�\B	�1B	�B	� B	|�B	z�B	x�B	v�B	t�B	q�B	n�B	n�B	l�B	jB	ffB	aHB	^5B	]/B	[#B	XB	S�B	P�B	K�B	J�B	H�B	H�B	G�B	G�B	F�B	E�B	C�B	?}B	<jB	;dB	;dB	:^B	:^B	:^B	:^B	:^B	:^B	:^B	:^B	:^B	:^B	:^B	9XB	9XB	9XB	9XB	8RB	7LB	7LB	6FB	5?B	5?B	49B	49B	49B	49B	49B	33B	33B	33B	2-B	33B	33B	2-B	33B	33B	33B	33B	33B	2-B	33B	33B	49B	33B	33B	2-B	2-B	5?B	7LB	7LB	8RB	7LB	7LB	7LB	7LB	6FB	6FB	6FB	6FB	6FB	6FB	5?B	5?B	5?B	5?B	5?B	5?B	49B	49B	5?B	5?B	5?B	49B	2-B	49B	8RB	>wB	@�B	A�B	B�B	B�B	C�B	C�B	D�B	D�B	E�B	E�B	E�B	E�B	F�B	F�B	G�B	G�B	G�B	H�B	G�B	H�B	K�B	P�B	S�B	T�B	VB	W
B	W
B	XB	YB	ZB	ZB	[#B	ZB	[#B	]/B	]/B	]/B	]/B	\)B	\)B	[#B	[#B	ZB	[#B	[#B	\)B	[#B	[#B	aHB	dZB	iyB	m�B	n�B	q�B	u�B	u�B	v�B	w�B	y�B	{�B	{�B	z�B	y�B	w�B	x�B	z�B	�B	�+B	�=B	�VB	�hB	�oB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	�9B	�RB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�)B	�5B	�;B	�HB	�TB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
	7B
	7B
JB
PB
oB
�B
�B
&�B
/B
2-B
2-B
33B
33B
33B
49B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
?}B
@�B
C�B
E�B
G�B
H�B
H�B
J�B
M�B
M�B
M�B
M�B
O�B
P�B
P�B
Q�B
S�B
W
B
W
B
XB
\)B
]/B
]/B
]/B
^5B
^5B
_;B
bNB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
ffB
gmB
gmB
hsB
hsB
iyB
jB
jB
k�B
m�B
m�B
m�B
m�B
o�B
o�B
p�B
p�B
q�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
z�B
|�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�PB
�VB
�oB
�{B
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
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
��B
��B
��B
��B
��B
�B
�'B
�B
�B
�3B
�3B
�9B
�3B
�3B
�FB
�RB
�^B
�dB
�jB
�jB
ÖB
��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��BBBBBBBBB��B��BB
=BVBoBoBuB�B�B"�B#�B$�B$�B$�B$�B$�B%�B%�B'�B0!B1'B1'B/B<jB;dB;dB9XB,B�B�B�B��B�B��B�B��B��B�PBt�B`ABW
BI�B6FB.B$�BhB  B�B�`B�AB�B��B�^B��B�{B�\B�DB�+B�Bv�B`ABK�BA�B9XB33B-B$�BuB
�B
�5B
��B
ŢB
�wB
�dB
�?B
��B
��B
��B
�JB
�B
y�B
u�B
p�B
hsB
cTB
\)B
O�B
F�B
@�B
9XB
(�B
�B
�B
\B
B	��B	��B	��B	�B	�B	�mB	�
B	��B	ŢB	�jB	�LB	�FB	�?B	�9B	�3B	�-B	�-B	�'B	�'B	�!B	�B	��B	��B	�{B	�oB	�hB	�VB	�JB	�7B	�B	x�B	t�B	n�B	k�B	gmB	cTB	_;B	ZB	T�B	P�B	O�B	N�B	M�B	J�B	G�B	C�B	@�B	;dB	33B	,B	'�B	#�B	 �B	�B	�B	�B	�B	�B	oB	oB	bB	VB	
=B	B	B	B��B��B��B��B�B�B�B�B�B�B�B�yB�mB�TB�AB�;B�;B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�5B�/B�/B�/B�/B�)B�#B�#B�B�B�B�B�B�B�B�B�
B�
B�
B�B�
B�
B�B�
B�
B�
B�
B�
B�B�
B�
B�B�
B�
B�B�B�B�#B�#B�)B�#B�#B�#B�#B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�)B�NB�ZB�`B�fB�fB�mB�mB�sB�sB�yB�yB�yB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	  B	  B��B��B��B��B��B	  B��B��B	B	1B	PB	hB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	+B	.B	2-B	5?B	6FB	7LB	7LB	7LB	9XB	:^B	?}B	@�B	E�B	I�B	J�B	XB	\)B	bNB	o�B	r�B	s�B	s�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	v�B	w�B	x�B	y�B	y�B	z�B	z�B	z�B	{�B	|�B	}�B	}�B	� B	�B	�B	�B	�+B	�=B	�DB	�DB	�JB	�VB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�FB	�XB	�wB	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�AB	�TB	�ZB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B

=B
DB
DB
JB
JB
PB
VB
VB
\B
hB
hB
hB
hB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
$�B
%�B
&�B
'�B
'�B
(�B
(�B
'�B
(�B
'�B
(�B
)�B
-B
.B
.B
.B
.B
.B
.B
.B
.B
/B
1'B
2-B
6FB
8RB
:^B
<jB
<jB
=qB
=qB
>wB
?}B
@�B
?}B
A�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
E�B
H�B
N�B
N�B
M�B
L�B
L�B
L�B
M�B
L�B
M�B
N�B
O�B
O�B
M�B
M�B
K�B
K�B
M�B
Q�B
T�B
O�B
O�B
W
B
W
B
XB
W
B
W
B
ZB
\)B
^5B
_;B
`AB
`AB
gmB
r�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                  ! * ) ' % % !                                                                                                                                                                                                                                                                                                                                                                                                                      ��000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000   PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0900000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 05 10 2018 117 -0.0900000 0.0040 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                        20230608223112              20230608223112              AO  ARCAADJP                                                                    20230608223112    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230608223112    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230608223112  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230608223112  QCF$                G�O�G�O�G�O�8000            