CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:18Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170918  20220204114421  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               hA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��v�1   @������@7�\(���c1?|�h1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    hA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D��D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�D�D�d�D��D��D�˅D�]�D��{D���D�%�D�VD���D��D�%qD�Y�D�b�D��)D�=D�D)D�|{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @|��@�  @�  A  A<  A\  A|  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B'  B/  B7  B?  BG  BO  BW  B_  BgffBo  Bw  B  B�L�B�� B�� B�� B�� B�� B�� B��3B�� B�� B�� B�� B�� B�� B�� B�� BÀ Bǀ Bˀ Bπ BӀ B׳3Bۀ B߀ B� B� B� B� B� B�� B�� B�� C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3ٚC5� C7� C9�fC;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� CqٚCs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D p D � Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D	p D	� D
p D
� Dp D� Dp D�Dp D� Dp D� Dp D� Dp D�Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� Dp D� D p D � D!p D!� D"p D"� D#p D#� D$p D$� D%p D%� D&p D&� D'p D'� D(p D(� D)p D)� D*p D*� D+p D+� D,p D,� D-p D-� D.p D.� D/p D/� D0p D0� D1p D1� D2p D2� D3p D3� D4p D4� D5p D5� D6p D6� D7p D7� D8p D8� D9p D9� D:p D:� D;p D;� D<p D<� D=p D=� D>p D>� D?p D?� D@p D@� DAp DA� DBp DB� DCp DC� DDp DD� DEp DE� DFp DF� DGp DG� DHp DH� DIp DI� DJp DJ� DKp DK� DLp DL� DMp DM� DNp DN� DOp DO� DPp DP� DQp DQ� DRp DR� DSp DS� DTp DT� DUp DU� DVp DV� DWp DW� DXp DX� DYp DY� DZp DZ� D[p D[� D\p D\� D]p D]� D^p D^� D_p D_� D`i�D`� Dap Da� Dbp Db� Dcp Dc� Ddp Dd� Dep De� Dfp Df� Dgp Dg� Dhp Dh� Dip Di� Djp Dj� Dkp Dk� Dli�Dl� Dmp Dm� Dnp Dn� Dop Do� Dpp Dp� Dqp Dq� Drp Dr� Dsp Ds� Dtp Dt� Dy�D�D�\�D��D��D�ÅD�U�D��{D���D��D�ND���D��D�qD�Q�D�Z�D��)D�=D�<)D�t{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AڮAڮAڰ!Aڰ!Aڰ!Aڲ-Aڲ-AڬAڥ�Aڰ!AڸRAڲ-Aڧ�Aڰ!Aڴ9AڼjAڸRAڬAڙ�AړuA�|�A�^5A�C�A�5?A��A���A�|�A֑hA�A�$�A�v�A�O�A��A�r�A�l�A�\)A��/AĬA�5?A��A�S�A��A�1'A���A�ȴA�+A��+A���A�E�A��FA�XA���A�E�A���A��`A�XA�%A��A��+A��A��9A�l�A�
=A���A�;dA��yA�x�A��A��uA���A���A�l�A�5?A��;A�bNA��A��A��#A��FA�E�A��uA� �A���A��#A�ȴA�C�A���A���A�E�A��FA��+A�33A�`BA��A���A�VA���A��yA�ZA���A�JA��A��
A�A�E�A�/A��jA�?}A���A���A�C�A��wA��A��`A�K�A�G�A��A��^A�M�A��hA�  A���A��hA�9XA�ĜA���A�(�A��A��A��A�
=A�VA��A�hsAhsA}��Az�DAw�PAu��AuXAt�+Arv�ApJAk�Af^5Ad^5AbA�A_�A]��AY%AV�AU�^AT�/AS�;AR��AP�!AOO�ALffAK?}AJVAH�AG��AEx�AB�yAA"�A?�A=�FA<-A:��A:JA9�A9;dA8bNA7|�A6�\A5?}A41A2��A1�wA1XA0�A/p�A.-A,-A*A�A)%A(~�A((�A'�-A';dA&Q�A$^5A#�7A"�uA 1'A��An�A��AA��A��A%A&�A�9A�A&�A��Az�AQ�A1'A�;AA=qA��A��AO�A�At�AA�A;dA��Av�AhsA	ƨA	+A{A��AG�A�/A�DA�wAx�A��A  A��A�A��A&�@��R@���@�Z@�A�@�
=@�7L@�z�@��@�7L@���@�@�F@���@�@�@�`B@�@@�K�@���@�E�@�j@ꟾ@�@畁@�;d@�+@���@�z�@��m@�|�@�|�@��m@�j@�j@�D@�D@�@�Q�@�w@�@�R@���@߅@�j@�;d@�X@ղ-@ԓu@ӍP@���@�p�@д9@��/@�o@��@���@�V@�E�@�Z@�b@Ǿw@�n�@��@���@�{@�\)@���@��h@�Q�@�Ĝ@�r�@���@�ȴ@��!@�"�@���@��
@�@�v�@�-@�5?@�@��^@��^@��@�=q@�J@��7@��j@�bN@��@�5?@�"�@�(�@�+@���@�b@�@�hs@�r�@�S�@��@��@��H@���@�Q�@��y@��h@�G�@��@��`@���@��9@�Z@�  @��
@��@�\)@��!@�@�p�@��`@���@�j@�Z@�(�@��@���@���@��@���@���@���@���@��w@�l�@��@��/@���@�%@��`@�bN@�S�@��@�33@�33@�+@�+@�+@�33@�+@��@�ȴ@��@�J@�=q@�J@��@��#@�@��-@���@���@��@�hs@��@��@�z�@�j@�A�@�  @��;@��
@��w@��w@���@�;d@�o@��@��+@�n�@�^5@�E�@�E�@�5?@�{@��T@���@�@��-@���@��7@��@�p�@�X@�O�@�O�@�O�@�G�@���@��@�bN@�I�@�1'@��
@���@�l�@�M�@�@��-@��^@�p�@�/@�X@��@��j@�Ĝ@��@�1@��
@��
@�ƨ@��@��@�l�@�
=@��R@��R@��R@���@���@���@��\@�E�@��T@��h@�O�@�G�@�%@��/@���@�9X@�9X@� �@��@���@��@��;@���@���@���@��6@z($@o�;@fTa@`]d@Yx�@R�@J�c@EJ�@>�@8�@0�4@(@$<�@�W@�K@�@@S�@s@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AڮAڮAڰ!Aڰ!Aڰ!Aڲ-Aڲ-AڬAڥ�Aڰ!AڸRAڲ-Aڧ�Aڰ!Aڴ9AڼjAڸRAڬAڙ�AړuA�|�A�^5A�C�A�5?A��A���A�|�A֑hA�A�$�A�v�A�O�A��A�r�A�l�A�\)A��/AĬA�5?A��A�S�A��A�1'A���A�ȴA�+A��+A���A�E�A��FA�XA���A�E�A���A��`A�XA�%A��A��+A��A��9A�l�A�
=A���A�;dA��yA�x�A��A��uA���A���A�l�A�5?A��;A�bNA��A��A��#A��FA�E�A��uA� �A���A��#A�ȴA�C�A���A���A�E�A��FA��+A�33A�`BA��A���A�VA���A��yA�ZA���A�JA��A��
A�A�E�A�/A��jA�?}A���A���A�C�A��wA��A��`A�K�A�G�A��A��^A�M�A��hA�  A���A��hA�9XA�ĜA���A�(�A��A��A��A�
=A�VA��A�hsAhsA}��Az�DAw�PAu��AuXAt�+Arv�ApJAk�Af^5Ad^5AbA�A_�A]��AY%AV�AU�^AT�/AS�;AR��AP�!AOO�ALffAK?}AJVAH�AG��AEx�AB�yAA"�A?�A=�FA<-A:��A:JA9�A9;dA8bNA7|�A6�\A5?}A41A2��A1�wA1XA0�A/p�A.-A,-A*A�A)%A(~�A((�A'�-A';dA&Q�A$^5A#�7A"�uA 1'A��An�A��AA��A��A%A&�A�9A�A&�A��Az�AQ�A1'A�;AA=qA��A��AO�A�At�AA�A;dA��Av�AhsA	ƨA	+A{A��AG�A�/A�DA�wAx�A��A  A��A�A��A&�@��R@���@�Z@�A�@�
=@�7L@�z�@��@�7L@���@�@�F@���@�@�@�`B@�@@�K�@���@�E�@�j@ꟾ@�@畁@�;d@�+@���@�z�@��m@�|�@�|�@��m@�j@�j@�D@�D@�@�Q�@�w@�@�R@���@߅@�j@�;d@�X@ղ-@ԓu@ӍP@���@�p�@д9@��/@�o@��@���@�V@�E�@�Z@�b@Ǿw@�n�@��@���@�{@�\)@���@��h@�Q�@�Ĝ@�r�@���@�ȴ@��!@�"�@���@��
@�@�v�@�-@�5?@�@��^@��^@��@�=q@�J@��7@��j@�bN@��@�5?@�"�@�(�@�+@���@�b@�@�hs@�r�@�S�@��@��@��H@���@�Q�@��y@��h@�G�@��@��`@���@��9@�Z@�  @��
@��@�\)@��!@�@�p�@��`@���@�j@�Z@�(�@��@���@���@��@���@���@���@���@��w@�l�@��@��/@���@�%@��`@�bN@�S�@��@�33@�33@�+@�+@�+@�33@�+@��@�ȴ@��@�J@�=q@�J@��@��#@�@��-@���@���@��@�hs@��@��@�z�@�j@�A�@�  @��;@��
@��w@��w@���@�;d@�o@��@��+@�n�@�^5@�E�@�E�@�5?@�{@��T@���@�@��-@���@��7@��@�p�@�X@�O�@�O�@�O�@�G�@���@��@�bN@�I�@�1'@��
@���@�l�@�M�@�@��-@��^@�p�@�/@�X@��@��j@�Ĝ@��@�1@��
@��
@�ƨ@��@��@�l�@�
=@��R@��R@��R@���@���@���@��\@�E�@��T@��h@�O�@�G�@�%@��/@���@�9X@�9X@� �@��@���@��@��;@���G�O�@���@��6@z($@o�;@fTa@`]d@Yx�@R�@J�c@EJ�@>�@8�@0�4@(@$<�@�W@�K@�@@S�@s@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
%B
B
B
B
B
B
B
%B
1B

=B
	7B
1B

=B
B
B
hB
uB
"�B
49B
?}B
J�B
N�B
jB
�B
�{B
��B
��B
�RB
�XB
�jB
ɺB
�;B
�B%B�B �B/BF�BS�BhsBx�B�B�B�oB��B��B�'B�^BB��B�B�BB�yB��BB	7BPBbB�B�B!�B#�B$�B%�B)�B.B-B,B+B(�B)�B-B49B7LB=qBE�BJ�BH�BG�BE�B@�B?}BA�B;dB8RB5?B6FB6FB6FB9XB5?B6FB0!B,B!�B{BhBVB��B�B�HBɺB�qB��B�{B�PB�7Bt�Bm�B`BB9XBB
�#B
��B
�qB
��B
�B
o�B
ZB
9XB
+B
bB	��B	�mB	�ZB	�BB	��B	ŢB	��B	�%B	t�B	jB	XB	I�B	2-B	�B	�B	uB	PB	+B��B��B�B�HB�5B�B��B�dB�B��B��B�hB�hB�bB�\B�hB�uB��B�{B�bB�DB�7B�+B�1B�+B�+B�B�B}�B~�B� B�B�%B�B�B�B� B{�B{�B� B}�B|�B{�B|�Bz�B{�B}�Bv�Bw�Bx�Bv�Bu�Bu�Bt�Bt�Bt�Bt�Bs�Bs�Bs�Br�Bu�Bt�Bv�Bv�Bv�Bv�Bx�Bw�Bw�Bx�Bu�Bs�Br�Br�Br�Br�Bu�Bu�Bu�Bu�Bu�Bw�Bw�Bv�Bu�Bu�Bv�Bu�Bw�By�B|�B�B�1B�7B�=B�PB�PB�PB�PB�PB�JB�JB�JB�JB�JB�DB�=B�=B�JB�bB�hB�uB��B��B��B�'B�XBBƨB��B��B��B��B��B��B��BƨB�}B�wB�FB�FB�RB�RB�9B�-B�?B��B��B��BÖB�dB�FB�9B�3B�B��B��B��B��B��B��B��B��B�!B�-B�-B�9B�RB�wB��B��B�B�#B�5B�;B�BB�HB�TB�sB�B�B�B�B��B	
=B	%B��B�B�B�B�B�B�yB�ZB�HB�B��B��B��B��B��B��B��B��B�B�
B�B�5B�;B�;B�HB�`B�yB�B�B�B�B�B��B��B��B��B��B��B	B	B	%B	JB	\B	{B	�B	�B	$�B	'�B	(�B	+B	.B	2-B	2-B	49B	7LB	9XB	:^B	;dB	;dB	:^B	;dB	<jB	B�B	H�B	K�B	M�B	P�B	Q�B	Q�B	R�B	R�B	R�B	S�B	W
B	ZB	[#B	_;B	dZB	jB	jB	l�B	p�B	r�B	u�B	x�B	{�B	}�B	�B	�B	�B	�B	�%B	�+B	�=B	�PB	�PB	�PB	�VB	�\B	�\B	�hB	�uB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�FB	�LB	�^B	�jB	�wB	�wB	�wB	�}B	�}B	�}B	�}B	��B	ÖB	ŢB	ƨB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�sB	�PB

=B
�B
&fB
,WB
6�B
9�B
>BB
EmB
IRB
P.B
RoB
Y�B
]~B
bB
e�B
j0B
n�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�XB	�XB	�XB	�XB	�XB	�XB	�QB	�QB	�KB	�XB	�^B	�dB	�XB	�^B	�^B	�dB	�^B	�^B	�XB	�QB	�KB	�QB	�dB	�pB
 |B	�vB	�pB
 |B	�RB	�ZB
�B
	�B
B
*xB
5�B
@�B
EB
`�B
{YB
��B
��B
�4B
��B
��B
��B
��B
�pB
�B
�WB�B�B%JB<�BJ%B^�Bo Bw0B{IB��B��B�#B�NB��B��B��B�(B�fBߜB��B�:B�XBqB�B�B�B�B�B�BB B$3B#-B"'B!!BB B#.B*XB-kB3�B;�B@�B>�B=�B;�B6�B5�B7�B1�B.sB+aB,hB,hB,hB/zB+aB,hB&DB"+B�B
�B�B|B�B��B�rB��B��B�B��B��BiBj�Bc�BVxB/�B
�DB
�dB
�B
��B
�)B
xVB
e�B
PjB
/�B
!SB
�B	�7B	��B	ڲB	֚B	�QB	��B	�NB	|�B	kB	`�B	NuB	@ B	(�B	)B	�B		�B	�B��B�\B�8B��B׹BԦB�vB�4B��B�~B�/B��B��B��B��B��B��B��B�B��B��B��B�B}�B~�B}�B}�Bx�Bw�BtqBuwBv}B{�B|�Bz�B{�By�Bv~BreBrfBvBtsBsmBrfBsmBqaBrgBttBmJBnPBoVBmJBlDBlDBk=Bk=Bk>Bk>Bj8Bj8Bj8Bi2BlEBk>BmKBmKBmLBmLBoXBnRBnRBoXBlGBj:Bi4Bi4Bi4Bi4BlGBlGBlGBlGBlGBnSBnTBmNBlHBlHBmNBlHBnTBp`BssB{�B~�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�[B��B��B�B�)B�ZB�xB�rB�rB�xB�mB�HB�*B� B��B��B��B��B��B��B��B��B�DB�OB�[B�B��B��B��B��B��B�|B�^B�XB�EB�FB�SB�MB�wB��B��B��B��B��B��B�LB�vBϛBѧBԸBվB��B��B��B��B�B�&B�,B�2B�uB	 �B��B�QB�3B�-B�B�B�	B��B��B��BϜB�rB�eB�TB�ZB�sB�yB�~B˄B̊B͐BУBԻB��B��B��B��B��B�B�)B�/B�5B�;B�GB�MB�SB�YB�eB�xB��B��B��B	�B	�B	
�B	B	:B	^B	qB	wB	!�B	$�B	(�B	(�B	*�B	-�B	/�B	0�B	1�B	1�B	0�B	1�B	2�B	9B	?3B	BFB	DRB	GcB	HjB	HjB	IpB	IpB	IpB	JvB	M�B	P�B	Q�B	U�B	Z�B	`�B	`�B	cB	g B	i,B	l?B	oQB	rbB	toB	w�B	y�B	z�B	{�B	|�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�>B	�>B	�PB	�iB	�uB	�uB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�+B	�+B	�1B	�8B	�=B	�CB	�CB	�CB	�OB	�[B	�UG�O�B	�RB	��B	��B
 �B
OB
�B
"�B
-B
02B
4�B
;�B
?�B
F�B
H�B
P>B
S�B
X�B
\B
`�B
eB
ga111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.25 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200619170918    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170918  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170918  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                