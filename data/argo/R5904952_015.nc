CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:08Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190508  20181005190508  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ר���0|1   @ר�:��@3��Q��c��l�C�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @333@�  @�  A   A   A@  A`  A�  A�  A���A���A�  A�  A�  A�  B   B  B  B��B��B'��B/��B8  B?��BH  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:�C<�C=�fC@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp�Cr�Ct�Cv  Cx  Cz�C|  C~  C�fC�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C��C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D�fDfD�fDfD�fD  D� D  D� D	  D	� D
  D
y�D  D� D  D�fDfD� D  D� D��D� D  Dy�D  D�fDfD� D  D� D  D� D  D�fD  D� D  D� D  Dy�D  D�fD  D� D��D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D"��D#y�D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)fD)� D*  D*�fD+  D+� D+��D,y�D,��D-y�D.  D.�fD/fD/�fD0  D0� D1  D1� D1��D2y�D3  D3y�D3��D4y�D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:y�D:��D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@  D@�fDAfDA�fDB  DB� DC  DC�fDD  DDy�DE  DE�fDFfDF� DG  DG� DG��DH� DI  DI� DJfDJ�fDKfDK�fDLfDL�fDM  DM� DM��DN� DOfDO�fDPfDP�fDQfDQ�fDR  DRy�DS  DS� DTfDT� DT��DU� DVfDV� DV��DW� DX  DX� DY  DYy�DY��DZ� D[fD[�fD\  D\� D]  D]� D^  D^�fD_fD_� D`fD`�fDafDa� Da��Db� DcfDc�fDd  Ddy�Dd��De� DffDf� Dg  Dg� DhfDh� Dh��Di� DjfDj� Dj��Dky�Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� DrfDr� Ds  Dsy�Dt  Dt� Dt��Du� Dv  Dv� DwfDw� Dw� Dy��D�1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @<��@���@���AffA"ffABffAbffA�33A�33A�  A�  A�33A�33A�33A�33B ��B��B��B34B 34B(34B034B8��B@34BH��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC&fC&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6@ C8@ C:@ C<@ C>�C@&fCB&fCD&fCF&fCH&fCJ&fCL&fCN@ CP&fCR&fCT&fCV&fCX&fCZ�C\&fC^&fC`&fCb&fCd&fCf&fCh&fCj@ Cl&fCn&fCp@ Cr@ Ct@ Cv&fCx&fCz@ C|&fC~&fC�fC�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�  C�  C�3C�3C�3C�3C�3C�3C�fC�3C�  C�  C�3C�3C�fC�3C�  C�3C�3C�3C�fC�3C�  C�3C�fC�3C�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�  C�3C�3C�3C�  C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�  C�3C�fC�3C�3C�3C�3C�3C�3C�  C�3C�3C�3C�3D 	�D ��D4D��D	�D��D	�D��D	�D� D D� D D� D	�D��D	�D��D		�D	��D
	�D
�4D	�D��D	�D� D D��D	�D��D4D��D	�D�4D	�D� D D��D	�D��D	�D��D	�D� D	�D��D	�D��D	�D�4D	�D� D	�D��D4D��D	�D��D	�D��D	�D��D D��D 	�D ��D!	�D!��D"	�D"��D#4D#�4D$	�D$��D%	�D%��D&	�D&��D'	�D'��D(4D(��D) D)��D*	�D*� D+	�D+��D,4D,�4D-4D-�4D.	�D.� D/ D/� D0	�D0��D1	�D1��D24D2�4D3	�D3�4D44D4�4D5	�D5��D64D6��D7	�D7��D8	�D8��D9	�D9��D:	�D:�4D;4D;��D<	�D<��D=	�D=�4D>	�D>��D?	�D?��D@	�D@� DA DA� DB	�DB��DC	�DC� DD	�DD�4DE	�DE� DF DF��DG	�DG��DH4DH��DI	�DI��DJ DJ� DK DK� DL DL� DM	�DM��DN4DN��DO DO� DP DP� DQ DQ� DR	�DR�4DS	�DS��DT DT��DU4DU��DV DV��DW4DW��DX	�DX��DY	�DY�4DZ4DZ��D[ D[� D\	�D\��D]	�D]��D^	�D^� D_ D_��D` D`� Da Da��Db4Db��Dc Dc� Dd	�Dd�4De4De��Df Df��Dg	�Dg��Dh Dh��Di4Di��Dj Dj��Dk4Dk�4Dl	�Dl� Dm	�Dm��Dn	�Dn��Do	�Do��Dp Dp��Dq	�Dq��Dr Dr��Ds	�Ds�4Dt	�Dt��Du4Du��Dv	�Dv��Dw Dw��Dw�Dy�{D�6�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A͙�A͗�A͗�A͛�A͝�Aͣ�Aͣ�Aͣ�Aͧ�Aͧ�Aͥ�AͬAͬAͧ�AͮAͲ-Aͺ^AͼjA���AͼjA���A�ĜAͥ�A͡�A͇+A���A���Ať�A�Q�A��Aě�A�M�A��A���AÛ�AÃAÃA�(�A�A���A��A��yA��uA�ffA���A���A��!A��A�/A��PA���A���A���A���A�I�A�ĜA�Q�A��A�?}A��\A�-A�Q�A�jA�^5A�x�A�  A�v�A�|�A��mA�1'A��FA��A�r�A��A���A���A���A���A�E�A�r�A�S�A�I�A��
A�S�A��!A� �A���A���A�E�A��A��A|�A{�;Ayp�AwAtbArVAp5?AoG�Alr�AiK�Ad �Ab$�AadZAa;dA`-A]XA[x�AX�9AVȴAT��AP��AM��AIdZAG�AGoAFQ�AE?}ADQ�AC�^AB1'A@M�A?O�A>�yA>�A=O�A<1A:�A:n�A:(�A9�mA9�A81A7;dA6(�A3�#A2�9A2�A0bA.�!A.VA.jA-\)A,~�A+�A(Q�A'+A%��A%K�A$Q�A#��A"�yA"E�A!��A r�A`BA�
AG�A  A+A��A��A��A�hA|�A�`A|�A�/Az�A�A�jA��A/A��A^5A��A/A
�A	�#A	O�AQ�A-AE�AffA�\AbNA;dA`B@�ff@�`B@���@���@��y@���@�33@�^5@��@�?}@���@�!@��@��/@�@�D@�D@�r�@��@��@�!@�p�@�@�D@�(�@��y@�I�@��m@�w@�+@��#@��@�9X@�+@�n�@�r�@�ff@׾w@��@ӶF@�ff@щ7@�&�@�Ĝ@���@�\)@�V@�p�@��@˾w@���@�t�@ʧ�@��@ȃ@Ǖ�@�K�@��@Ə\@���@�7L@��@å�@���@���@���@�b@��@�"�@��+@�^5@��#@�%@�j@���@�K�@�=q@��-@��@��@�
=@���@�X@���@��9@��u@�z�@��P@���@���@��\@��+@�v�@�M�@�@�O�@��@�$�@���@��y@��@��j@��@���@���@���@���@�^5@���@��@��@�$�@�o@��@��@�E�@�{@��@���@��h@�O�@�7L@���@�bN@�Q�@�Z@�9X@�ƨ@���@�~�@��@�V@�@�~�@���@��H@�o@�"�@�+@�+@�33@�33@�33@�@���@�p�@��@�ƨ@�~�@���@��T@��@���@��@�-@�E�@�E�@�@��@��@�t�@�\)@�33@��H@���@��!@���@��R@��!@��!@���@��+@�E�@���@�/@�%@��`@���@�z�@�Q�@�(�@��@��@��F@�t�@��@���@��+@�^5@�E�@��#@�O�@�V@���@��j@�j@�I�@�9X@�b@�l�@�@�+@�"�@��@�o@�@��y@��H@���@�^5@�5?@��T@���@��7@�?}@��`@��@�z�@�Z@�I�@�1'@�  @�\)@�ȴ@�n�@�M�@���@�p�@�hs@�X@�O�@�/@���@�1'@���@���@��P@�l�@��@���@���@��+@���@��+@�V@��^@���@��u@��D@�z�@�Q�@�1'@��@��F@�|�@�\)@�C�@�
=@��y@�ȴ@���@�V@���@���@�G�@��@���@���@�A�@�  @��m@���@���@�|�@�C�@�o@�v�@���@�/@��j@� �@�+@���@��R@���@�H@x�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A͙�A͗�A͗�A͛�A͝�Aͣ�Aͣ�Aͣ�Aͧ�Aͧ�Aͥ�AͬAͬAͧ�AͮAͲ-Aͺ^AͼjA���AͼjA���A�ĜAͥ�A͡�A͇+A���A���Ať�A�Q�A��Aě�A�M�A��A���AÛ�AÃAÃA�(�A�A���A��A��yA��uA�ffA���A���A��!A��A�/A��PA���A���A���A���A�I�A�ĜA�Q�A��A�?}A��\A�-A�Q�A�jA�^5A�x�A�  A�v�A�|�A��mA�1'A��FA��A�r�A��A���A���A���A���A�E�A�r�A�S�A�I�A��
A�S�A��!A� �A���A���A�E�A��A��A|�A{�;Ayp�AwAtbArVAp5?AoG�Alr�AiK�Ad �Ab$�AadZAa;dA`-A]XA[x�AX�9AVȴAT��AP��AM��AIdZAG�AGoAFQ�AE?}ADQ�AC�^AB1'A@M�A?O�A>�yA>�A=O�A<1A:�A:n�A:(�A9�mA9�A81A7;dA6(�A3�#A2�9A2�A0bA.�!A.VA.jA-\)A,~�A+�A(Q�A'+A%��A%K�A$Q�A#��A"�yA"E�A!��A r�A`BA�
AG�A  A+A��A��A��A�hA|�A�`A|�A�/Az�A�A�jA��A/A��A^5A��A/A
�A	�#A	O�AQ�A-AE�AffA�\AbNA;dA`B@�ff@�`B@���@���@��y@���@�33@�^5@��@�?}@���@�!@��@��/@�@�D@�D@�r�@��@��@�!@�p�@�@�D@�(�@��y@�I�@��m@�w@�+@��#@��@�9X@�+@�n�@�r�@�ff@׾w@��@ӶF@�ff@щ7@�&�@�Ĝ@���@�\)@�V@�p�@��@˾w@���@�t�@ʧ�@��@ȃ@Ǖ�@�K�@��@Ə\@���@�7L@��@å�@���@���@���@�b@��@�"�@��+@�^5@��#@�%@�j@���@�K�@�=q@��-@��@��@�
=@���@�X@���@��9@��u@�z�@��P@���@���@��\@��+@�v�@�M�@�@�O�@��@�$�@���@��y@��@��j@��@���@���@���@���@�^5@���@��@��@�$�@�o@��@��@�E�@�{@��@���@��h@�O�@�7L@���@�bN@�Q�@�Z@�9X@�ƨ@���@�~�@��@�V@�@�~�@���@��H@�o@�"�@�+@�+@�33@�33@�33@�@���@�p�@��@�ƨ@�~�@���@��T@��@���@��@�-@�E�@�E�@�@��@��@�t�@�\)@�33@��H@���@��!@���@��R@��!@��!@���@��+@�E�@���@�/@�%@��`@���@�z�@�Q�@�(�@��@��@��F@�t�@��@���@��+@�^5@�E�@��#@�O�@�V@���@��j@�j@�I�@�9X@�b@�l�@�@�+@�"�@��@�o@�@��y@��H@���@�^5@�5?@��T@���@��7@�?}@��`@��@�z�@�Z@�I�@�1'@�  @�\)@�ȴ@�n�@�M�@���@�p�@�hs@�X@�O�@�/@���@�1'@���@���@��P@�l�@��@���@���@��+@���@��+@�V@��^@���@��u@��D@�z�@�Q�@�1'@��@��F@�|�@�\)@�C�@�
=@��y@�ȴ@���@�V@���@���@�G�@��@���@���@�A�@�  @��m@���@���@�|�@�C�@�o@�v�@���@�/@��j@� �@�+@���@��R@���@�H@x�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
k�B
k�B
k�B
l�B
l�B
o�B
n�B
n�B
n�B
p�B
q�B
p�B
r�B
q�B
v�B
x�B
� B
�B
�=B
�B
��B
�B
�RB
�XBt�B��B��B��B�?BƨB��B�HB�B�BBBB{B)�BC�BW
BO�BT�BaHBaHBx�B�B�DB�DB�hB��B��B��B��B�oBz�BcTBW
BP�BH�B=qB/B&�B�B�B�BhBPB��B��B�XB�By�BN�B5?B'�B�BoBPBB
�B
�TB
��B
�B
��B
��B
�B
gmB
B�B
1'B
�B
PB
B	�B	�ZB	��B	ÖB	�^B	�'B	��B	�=B	jB	^5B	ZB	XB	P�B	B�B	9XB	+B	!�B	�B	+B��B�5B�B�#B�#B�B�/B�)B��B��B��BȴB��B�BB�NB�`B�fB�fB�`B�`B�yB�yB�TB��BȴBB�?B��B�LB��B�`B�sB�NB�#B�HB�/B�5B�/B�B��B��BȴBĜB��B�)B�#B�NB�`B�B�`B�5B�BB�B��B��B��B	  B	B	B	B	B��B��B��B�B�sB�mB�TB�5B�5B�HB�TB�mB�`B�5B�}B�!B�B��B��B�uB�VB�PB�PB�PB�VB�VB�VB�VB�VB�VB�\B�\B�VB�\B�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�RB�}B��BBŢBŢBŢBŢBƨB��B��B��B�B�5B�fB�B�B�B�B�B�B�B�B��B��B��B��B��B	B	1B		7B	PB	�B	�B	�B	�B	#�B	%�B	(�B	-B	33B	;dB	>wB	@�B	C�B	D�B	E�B	L�B	Q�B	R�B	R�B	R�B	R�B	R�B	T�B	_;B	cTB	l�B	r�B	t�B	n�B	m�B	q�B	v�B	}�B	z�B	w�B	w�B	w�B	x�B	z�B	}�B	�%B	�DB	�JB	�\B	�\B	�\B	�bB	�bB	�bB	�\B	�\B	�bB	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�3B	�3B	�3B	�3B	�9B	�FB	�RB	�}B	��B	�}B	�qB	�qB	�}B	��B	��B	B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
%B
%B
%B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB

=B
DB

=B
	7B

=B
DB
DB
JB
~B
�2222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
k�B
k�B
k�B
l�B
l�B
o�B
n�B
n�B
n�B
p�B
q�B
p�B
r�B
q�B
v�B
x�B
� B
�B
�=B
�B
��B
�B
�RB
�XBt�B��B��B��B�?BƨB��B�HB�B�BBBB{B)�BC�BW
BO�BT�BaHBaHBx�B�B�DB�DB�hB��B��B��B��B�oBz�BcTBW
BP�BH�B=qB/B&�B�B�B�BhBPB��B��B�XB�By�BN�B5?B'�B�BoBPBB
�B
�TB
��B
�B
��B
��B
�B
gmB
B�B
1'B
�B
PB
B	�B	�ZB	��B	ÖB	�^B	�'B	��B	�=B	jB	^5B	ZB	XB	P�B	B�B	9XB	+B	!�B	�B	+B��B�5B�B�#B�#B�B�/B�)B��B��B��BȴB��B�BB�NB�`B�fB�fB�`B�`B�yB�yB�TB��BȴBB�?B��B�LB��B�`B�sB�NB�#B�HB�/B�5B�/B�B��B��BȴBĜB��B�)B�#B�NB�`B�B�`B�5B�BB�B��B��B��B	  B	B	B	B	B��B��B��B�B�sB�mB�TB�5B�5B�HB�TB�mB�`B�5B�}B�!B�B��B��B�uB�VB�PB�PB�PB�VB�VB�VB�VB�VB�VB�\B�\B�VB�\B�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�RB�}B��BBŢBŢBŢBŢBƨB��B��B��B�B�5B�fB�B�B�B�B�B�B�B�B��B��B��B��B��B	B	1B		7B	PB	�B	�B	�B	�B	#�B	%�B	(�B	-B	33B	;dB	>wB	@�B	C�B	D�B	E�B	L�B	Q�B	R�B	R�B	R�B	R�B	R�B	T�B	_;B	cTB	l�B	r�B	t�B	n�B	m�B	q�B	v�B	}�B	z�B	w�B	w�B	w�B	x�B	z�B	}�B	�%B	�DB	�JB	�\B	�\B	�\B	�bB	�bB	�bB	�\B	�\B	�bB	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�3B	�3B	�3B	�3B	�9B	�FB	�RB	�}B	��B	�}B	�qB	�qB	�}B	��B	��B	B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
%B
%B
%B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB

=B
DB

=B
	7B

=B
DB
DB
JB
~B
�2222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190508                              AO  ARCAADJP                                                                    20181005190508    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190508  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190508  QCF$                G�O�G�O�G�O�8000            