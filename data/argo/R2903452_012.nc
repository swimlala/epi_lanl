CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-18T16:40:41Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    C�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    M�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  W�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    _�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  a�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    i�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  k�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    {�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  }�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                   ��   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230718164041  20230718164041  2903452 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL            NB_SAMPLE_CTD      A   AO  9473                            2B  A   APEX                            8931                            021122                          846 @�4���1   @�4�W:۬@2&Ov_��d=p��
1   GPS     Primary sampling: mixed [deep: discrete, shallow: averaged]                                                                                                                                                                                                        A   A   A       @9��@�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�
=D�UqD�� D���D�!�D�W�D���D���D�D�_
D���DǷ
D��D�<)DڝqD�� D��D�>D�fD��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @X��@��@ϮA�
A'�
AG�
Ag�
A��A��RA��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$�C&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D %�D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.�D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg��Dh%�Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Dt�\Dy��D��D�eD���D�ڏD�1�D�g\D��fD��D�,�D�n�D��RD�ƸD�&fD�K�DڭD�߮D�+�D�M�D�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�=qA�S�A�XA�XA�ZA�ZA�\)A�^5A�^5A�`BA�bNA�S�A�K�A�Q�A�VA�S�A�33A�$�A�oA��AҼjA�z�A�r�A�n�A�dZA�^5A�Q�A�I�A�C�A�9XA�;dA��A��#A���A���AѸRA��A�A�p�A�bNA�C�A�l�A�1'A�ƨA�;dA�ȴA�7LA�XA�|�Aŗ�A�O�A�7LA�/A�$�A�t�A��7A��`A��A�VA��A�?}A�S�A�+A�hsA��A�ȴA�Q�A�x�A�ƨA�A�33A��A�`BA���A�/A�|�A�  A�%A���A��TA��A�C�A�1A�1A�x�A��DA�;dA���A��DA��
A�l�A�oA�VA��DA�E�A��9A�A�A��hA��A��A�E�A���A�C�A��PA��7A�bA�
=A~�yA|��AxjAu��At��Aq�Anz�Ak�AiƨAh-AgXAfjAd��A`�yA_"�A\�HAX�HAVE�ATv�AS�#AQ�AP��AO�7AM��AKXAIl�AG��AC;dA@��A?��A>��A=A9�;A8(�A65?A5A3�TA2bA0^5A-t�A,�DA, �A+oA(��A'�#A'��A'dZA'XA%�A#ƨA#A!��A!`BA!oA �At�AƨAl�A�DAoAx�A`BA��A5?AM�A��A�AK�A=qAC�AAdZAS�AhsA��A��A��A��A��A�A
�uA��AS�A5?A$�A��A �A�yAȴA�+A�DA
$�A�AK�A
�uA	�;A	ƨA	%A��AVAn�A-A  A�A�A��A�+A1A�A �@�n�@���@���@�`B@��F@�"�@��H@��@��@�Z@���@�;d@��@�J@���@���@�w@��H@�@�@�-@��@���@���@��@�`B@��`@�ƨ@�l�@�l�@�"�@�R@���@�`B@���@�  @�dZ@���@�`B@�O�@�p�@�x�@��#@�$�@��H@�33@�|�@�ȴ@�7@�j@��@��;@� �@���@��@�R@�M�@�@�p�@�9@��u@�Z@��@�\)@�C�@��@�M�@��@�hs@��`@��@�@ڗ�@�M�@٩�@���@�ƨ@�A�@�r�@֧�@���@�@ёh@Ұ!@�
=@�K�@�|�@ӕ�@�;d@�v�@с@�X@�I�@��;@��;@ϥ�@�;d@�^5@�{@̃@�O�@ȴ9@ȣ�@ȋD@���@ǥ�@�$�@�x�@�%@���@�n�@��T@�X@��@��@���@��@�K�@��y@�n�@�E�@���@���@��@���@�@���@��h@��@���@���@��R@�^5@���@���@�ƨ@�ȴ@�v�@�^5@�E�@��+@�&�@�A�@�1@�Q�@�E�@�5?@�-@�J@�@�G�@��j@�j@�\)@���@�J@��7@�x�@��@�Ĝ@���@��u@��u@���@��/@���@� �@�@��+@�$�@��^@��7@�/@�V@���@���@�9X@��F@���@�M�@�@��@��h@���@���@��u@�bN@�A�@� �@�  @���@��;@�l�@��y@�v�@��T@���@���@��@���@��@�33@���@��!@��!@��\@�^5@�=q@��T@��-@�`B@���@��u@�Z@�(�@��m@�K�@��\@�V@���@���@���@��u@�I�@��@�o@���@�ff@��T@�p�@�7L@�%@�Ĝ@�z�@�bN@�(�@��w@�t�@�S�@�S�@�C�@���@��+@�$�@�@��@���@�x�@�/@���@�j@�(�@��
@���@��@�
=@���@���@�n�@�=q@�J@��^@��-@�x�@�O�@���@��D@�bN@�A�@���@�dZ@�\)@��@�_p@��@���@yF@l�@c��@Z6�@Q�h@F�L@?(@7�}@2!�@+.I@$��@�f@�9@��@>�@Ĝ@�<1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�5?A�=qA�S�A�XA�XA�ZA�ZA�\)A�^5A�^5A�`BA�bNA�S�A�K�A�Q�A�VA�S�A�33A�$�A�oA��AҼjA�z�A�r�A�n�A�dZA�^5A�Q�A�I�A�C�A�9XA�;dA��A��#A���A���AѸRA��A�A�p�A�bNA�C�A�l�A�1'A�ƨA�;dA�ȴA�7LA�XA�|�Aŗ�A�O�A�7LA�/A�$�A�t�A��7A��`A��A�VA��A�?}A�S�A�+A�hsA��A�ȴA�Q�A�x�A�ƨA�A�33A��A�`BA���A�/A�|�A�  A�%A���A��TA��A�C�A�1A�1A�x�A��DA�;dA���A��DA��
A�l�A�oA�VA��DA�E�A��9A�A�A��hA��A��A�E�A���A�C�A��PA��7A�bA�
=A~�yA|��AxjAu��At��Aq�Anz�Ak�AiƨAh-AgXAfjAd��A`�yA_"�A\�HAX�HAVE�ATv�AS�#AQ�AP��AO�7AM��AKXAIl�AG��AC;dA@��A?��A>��A=A9�;A8(�A65?A5A3�TA2bA0^5A-t�A,�DA, �A+oA(��A'�#A'��A'dZA'XA%�A#ƨA#A!��A!`BA!oA �At�AƨAl�A�DAoAx�A`BA��A5?AM�A��A�AK�A=qAC�AAdZAS�AhsA��A��A��A��A��A�A
�uA��AS�A5?A$�A��A �A�yAȴA�+A�DA
$�A�AK�A
�uA	�;A	ƨA	%A��AVAn�A-A  A�A�A��A�+A1A�A �@�n�@���@���@�`B@��F@�"�@��H@��@��@�Z@���@�;d@��@�J@���@���@�w@��H@�@�@�-@��@���@���@��@�`B@��`@�ƨ@�l�@�l�@�"�@�R@���@�`B@���@�  @�dZ@���@�`B@�O�@�p�@�x�@��#@�$�@��H@�33@�|�@�ȴ@�7@�j@��@��;@� �@���@��@�R@�M�@�@�p�@�9@��u@�Z@��@�\)@�C�@��@�M�@��@�hs@��`@��@�@ڗ�@�M�@٩�@���@�ƨ@�A�@�r�@֧�@���@�@ёh@Ұ!@�
=@�K�@�|�@ӕ�@�;d@�v�@с@�X@�I�@��;@��;@ϥ�@�;d@�^5@�{@̃@�O�@ȴ9@ȣ�@ȋD@���@ǥ�@�$�@�x�@�%@���@�n�@��T@�X@��@��@���@��@�K�@��y@�n�@�E�@���@���@��@���@�@���@��h@��@���@���@��R@�^5@���@���@�ƨ@�ȴ@�v�@�^5@�E�@��+@�&�@�A�@�1@�Q�@�E�@�5?@�-@�J@�@�G�@��j@�j@�\)@���@�J@��7@�x�@��@�Ĝ@���@��u@��u@���@��/@���@� �@�@��+@�$�@��^@��7@�/@�V@���@���@�9X@��F@���@�M�@�@��@��h@���@���@��u@�bN@�A�@� �@�  @���@��;@�l�@��y@�v�@��T@���@���@��@���@��@�33@���@��!@��!@��\@�^5@�=q@��T@��-@�`B@���@��u@�Z@�(�@��m@�K�@��\@�V@���@���@���@��u@�I�@��@�o@���@�ff@��T@�p�@�7L@�%@�Ĝ@�z�@�bN@�(�@��w@�t�@�S�@�S�@�C�@���@��+@�$�@�@��@���@�x�@�/@���@�j@�(�@��
@���@��@�
=@���@���@�n�@�=q@�J@��^@��-@�x�@�O�@���@��D@�bN@�A�@���@�dZ@�\)@��@�_p@��@���@yF@l�@c��@Z6�@Q�h@F�L@?(@7�}@2!�@+.I@$��@�f@�9@��@>�@Ĝ@�<1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�!B
ĜB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ȴB
ŢB
ǮB
ȴB
ȴB
ɺB
ȴB
ɺB
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
ɺB
�?B
��B
�B�B�BF�B9XB<jBD�BI�BG�B� B��B�{B��B��B�B�B�3B��B��B�BB%�B/B"�BhB\B�B	7B  B��B1BDB1B%BB�B�B�B��B��BÖB�^B�B�hBw�BjBZBM�B>wB{B
��B
�B
�mB
�BB
��B
�dB
��B
��B
��B
�hB
�+B
}�B
n�B
iyB
_;B
L�B
{B
B	�`B	�#B	��B	�RB	��B	��B	�=B	z�B	hsB	_;B	VB	P�B	H�B	D�B	33B	&�B	�B	1B��B��B��B�B�B�fB�ZB�/B��B��B��B�XB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�dB��B��B��B��B��B��B�5B�ZB��B��B	{B	�B	$�B	,B	5?B	C�B	6FB	33B	)�B	2-B	6FB	5?B	33B	%�B	�B	�B	{B	uB	{B		7B	B	B�B�5B��B��B�B	$�B	8RB	8RB	5?B	6FB	M�B	q�B	r�B	l�B	gmB	dZB	bNB	O�B	D�B	=qB	=qB	>wB	?}B	A�B	H�B	K�B	L�B	J�B	G�B	D�B	F�B	]/B	_;B	\)B	]/B	`BB	\)B	\)B	]/B	_;B	cTB	e`B	iyB	p�B	r�B	o�B	m�B	k�B	gmB	hsB	o�B	q�B	q�B	r�B	v�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	w�B	x�B	�B	�DB	�JB	�VB	�bB	��B	��B	��B	��B	�B	�'B	�!B	�'B	�9B	�LB	�qB	��B	�qB	�dB	�XB	�RB	�^B	�jB	�dB	�jB	�jB	�qB	�qB	�}B	B	ÖB	ŢB	ŢB	ĜB	ȴB	ȴB	ȴB	ɺB	ǮB	ŢB	��B	��B	ɺB	ĜB	�jB	�9B	�wB	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ĜB	��B	�^B	�LB	�LB	�RB	�wB	�^B	�?B	�-B	�3B	�3B	�B	�B	�B	�!B	�9B	�?B	�3B	�-B	�3B	�?B	�^B	�dB	�jB	�}B	�wB	�}B	��B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�TB	�ZB	�`B	�ZB	�ZB	�HB	�5B	�/B	�#B	�B	�
B	�B	�B	�#B	�)B	�/B	�/B	�;B	�NB	�TB	�ZB	�ZB	�TB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�fB	�mB	�mB	�fB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B
	7B

=B

=B
JB
�B
�B
�B
!�B
&B
+�B
3�B
<6B
E�B
LdB
QhB
VB
^jB
c�B
h�B
l�B
oiB
q�B
t�B
vF1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�B
�!B
ĜB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ƨB
ǮB
ȴB
ŢB
ǮB
ȴB
ȴB
ɺB
ȴB
ɺB
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
ɺB
�?B
��B
�B�B�BF�B9XB<jBD�BI�BG�B� B��B�{B��B��B�B�B�3B��B��B�BB%�B/B"�BhB\B�B	7B  B��B1BDB1B%BB�B�B�B��B��BÖB�^B�B�hBw�BjBZBM�B>wB{B
��B
�B
�mB
�BB
��B
�dB
��B
��B
��B
�hB
�+B
}�B
n�B
iyB
_;B
L�B
{B
B	�`B	�#B	��B	�RB	��B	��B	�=B	z�B	hsB	_;B	VB	P�B	H�B	D�B	33B	&�B	�B	1B��B��B��B�B�B�fB�ZB�/B��B��B��B�XB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�dB��B��B��B��B��B��B�5B�ZB��B��B	{B	�B	$�B	,B	5?B	C�B	6FB	33B	)�B	2-B	6FB	5?B	33B	%�B	�B	�B	{B	uB	{B		7B	B	B�B�5B��B��B�B	$�B	8RB	8RB	5?B	6FB	M�B	q�B	r�B	l�B	gmB	dZB	bNB	O�B	D�B	=qB	=qB	>wB	?}B	A�B	H�B	K�B	L�B	J�B	G�B	D�B	F�B	]/B	_;B	\)B	]/B	`BB	\)B	\)B	]/B	_;B	cTB	e`B	iyB	p�B	r�B	o�B	m�B	k�B	gmB	hsB	o�B	q�B	q�B	r�B	v�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	w�B	x�B	�B	�DB	�JB	�VB	�bB	��B	��B	��B	��B	�B	�'B	�!B	�'B	�9B	�LB	�qB	��B	�qB	�dB	�XB	�RB	�^B	�jB	�dB	�jB	�jB	�qB	�qB	�}B	B	ÖB	ŢB	ŢB	ĜB	ȴB	ȴB	ȴB	ɺB	ǮB	ŢB	��B	��B	ɺB	ĜB	�jB	�9B	�wB	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	ĜB	��B	�^B	�LB	�LB	�RB	�wB	�^B	�?B	�-B	�3B	�3B	�B	�B	�B	�!B	�9B	�?B	�3B	�-B	�3B	�?B	�^B	�dB	�jB	�}B	�wB	�}B	��B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�TB	�ZB	�`B	�ZB	�ZB	�HB	�5B	�/B	�#B	�B	�
B	�B	�B	�#B	�)B	�/B	�/B	�;B	�NB	�TB	�ZB	�ZB	�TB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�fB	�mB	�mB	�fB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B
	7B

=B

=B
JB
�B
�B
�B
!�B
&B
+�B
3�B
<6B
E�B
LdB
QhB
VB
^jB
c�B
h�B
l�B
oiB
q�B
t�B
vF1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                            " !             # # & % " "                    * 9 2 ) % !                                    $ $ "                         ' ) ( # "                                                                                                                                                                                                                                                                                                                                                  ����������������������0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999999999999999999999 PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.49 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230718164041                                          AO  ARCAADJP                                                                    20230718164041    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230718164041  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230718164041  QCF$                G�O�G�O�G�O�0               