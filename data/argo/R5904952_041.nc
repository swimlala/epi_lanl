CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ^   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:15Z creation      
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
resolution        =���   axis      Z        x  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  @H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  G    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  L�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  R   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  Sp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  X�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ZH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  _�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  e8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  f�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  mp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  r�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    s   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    v   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  |   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    |D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    |H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    |L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    |P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  |T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    |�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    |�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    |�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         |�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         |�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        |�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    |�Argo profile    3.1 1.2 19500101000000  20181005190515  20181005190515  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               )A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׹��O,1   @׹�q�p@1�|�hs�c������1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      )A   A   A   @@  @�33@�  A   A   A@  A`  A�  A�33A�33A�  A�  A�  A�  A�  B   B  B��B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C��3D   D � D ��D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  Dy�D��Dy�D  D� D  Dy�D��D� D  D� D  D� D  D� DfD� D  Dy�D  D� D  D� D  D� DfD� D  D� D  D�fDfD�fDfD�fDfD� D   D y�D!  D!� D"  D"� D#  D#y�D$  D$y�D$��D%y�D%��D&y�D'  D'� D(fD(�fD)  D)� D*  D*y�D+  D+� D+��D,� D-  D-y�Dy�{D�4�D�}q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @O\)@��G@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B�]B�]B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��By\)B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�BĮB�z�B�z�B�z�B�z�B�z�B�z�B�G�B�G�B�z�B�z�B�z�B�z�B��B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qCWC=qC=qC=qC =qC"=qC$=qC&WC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qClWCnWCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C�+�C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D�D�\D\D�\D\D�\D\D�\D\D�\D�D�\D\D�\D\D�\D	\D	�\D
�D
�\D\D�\D\D�\D\D��D�D��D\D�\D\D��D�D�\D\D�\D\D�\D\D�\D�D�\D\D��D\D�\D\D�\D\D�\D�D�\D\D�\D\D��D�D��D�D��D�D�\D \D ��D!\D!�\D"\D"�\D#\D#��D$\D$��D%�D%��D&�D&��D'\D'�\D(�D(��D)\D)�\D*\D*��D+\D+�\D,�D,�\D-\D-��Dy��D�<{D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A��A���A�A�A�A�A�%A�%A�1A�%A�%A�1A�1A�A�A��A���A�p�A��A��mA��/A��#A��A���A�ȴA�ĜA�A�AԺ^A԰!Aԧ�AԋDA�hsA�"�A��A���AӋDA�dZA�G�A�;dA�A�A�C�A�A�A�{A�hsAуAЙ�A�E�A�hsA���A�K�A�I�A�$�A��A�dZAɥ�A�n�A��AǛ�AœuAĸRA���A�l�A�S�A�;dA���A�E�A�A�dZA��A�JA��A��FA���A�C�A��A�
=A�oA�A�A��DA�K�A��!A��#A���A���A���A��A���A�7LA�  A�dZA���A��A�r�A���A�ȴA�33A�^5A���A�ȴA�=qA�jA�=qA�K�A��FA���A��hA�A�A���A~�A{l�AwC�As��AqK�Aol�Am��Ak��Ai7LAe��Ac"�A_;dA^��A^{A]�FA]p�A\�uAZ�yAX$�AU�;AU�PAUC�AS�mAP9XALbNAKC�AJ�AI;dAG�mAF�jAE��AD�RAC�TAB��AA�A@�yA?��A>ȴA<r�A;�hA:z�A8(�A5�
A3ƨA1��A0��A/7LA-��A,��A,I�A+��A+t�A)��A(Q�A&��A%�A$n�A#oA"Q�A!+AȴA
=A��AI�A|�A�uA��A�A�mA��At�Az�A��A��A  A
=A�A�A
��A	ƨA	oAE�A�A��AM�A�DA��AffA�;A�yAA�A&�A(�A�TA��Al�A�hA{@���@�o@�33@��+A 9XAdZA��AƨA;dA �j@�33@���@�7L@��@�n�@�v�@�~�@���@��@��#@�?}@�X@���@���@�I�@�@�j@�j@�x�@���@�^5@���@�%@���@�@�A�@���@���@�=q@���@�@�V@�bN@��`@޸R@�=q@�/@�1@���@�+@Χ�@�X@�/@�A�@�1@�b@� �@˝�@� �@�S�@�ȴ@�v�@���@�V@ȋD@ȴ9@�x�@Ɂ@ȓu@� �@Ɵ�@�@��@ċD@�1'@�A�@�j@���@�C�@���@�o@�ȴ@��@�=q@���@��9@���@���@�bN@�1'@���@��@��D@�|�@�~�@�$�@�ff@��T@�X@�G�@�&�@��D@���@�\)@�;d@��y@��!@���@���@�v�@�M�@��h@�`B@�O�@�x�@��@�M�@�M�@���@���@��@�v�@�V@�-@��@��h@�?}@���@��u@���@��@�@�v�@���@���@��^@���@���@{�@k�011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��A���A�A�A�A�A�%A�%A�1A�%A�%A�1A�1A�A�A��A���A�p�A��A��mA��/A��#A��A���A�ȴA�ĜA�A�AԺ^A԰!Aԧ�AԋDA�hsA�"�A��A���AӋDA�dZA�G�A�;dA�A�A�C�A�A�A�{A�hsAуAЙ�A�E�A�hsA���A�K�A�I�A�$�A��A�dZAɥ�A�n�A��AǛ�AœuAĸRA���A�l�A�S�A�;dA���A�E�A�A�dZA��A�JA��A��FA���A�C�A��A�
=A�oA�A�A��DA�K�A��!A��#A���A���A���A��A���A�7LA�  A�dZA���A��A�r�A���A�ȴA�33A�^5A���A�ȴA�=qA�jA�=qA�K�A��FA���A��hA�A�A���A~�A{l�AwC�As��AqK�Aol�Am��Ak��Ai7LAe��Ac"�A_;dA^��A^{A]�FA]p�A\�uAZ�yAX$�AU�;AU�PAUC�AS�mAP9XALbNAKC�AJ�AI;dAG�mAF�jAE��AD�RAC�TAB��AA�A@�yA?��A>ȴA<r�A;�hA:z�A8(�A5�
A3ƨA1��A0��A/7LA-��A,��A,I�A+��A+t�A)��A(Q�A&��A%�A$n�A#oA"Q�A!+AȴA
=A��AI�A|�A�uA��A�A�mA��At�Az�A��A��A  A
=A�A�A
��A	ƨA	oAE�A�A��AM�A�DA��AffA�;A�yAA�A&�A(�A�TA��Al�A�hA{@���@�o@�33@��+A 9XAdZA��AƨA;dA �j@�33@���@�7L@��@�n�@�v�@�~�@���@��@��#@�?}@�X@���@���@�I�@�@�j@�j@�x�@���@�^5@���@�%@���@�@�A�@���@���@�=q@���@�@�V@�bN@��`@޸R@�=q@�/@�1@���@�+@Χ�@�X@�/@�A�@�1@�b@� �@˝�@� �@�S�@�ȴ@�v�@���@�V@ȋD@ȴ9@�x�@Ɂ@ȓu@� �@Ɵ�@�@��@ċD@�1'@�A�@�j@���@�C�@���@�o@�ȴ@��@�=q@���@��9@���@���@�bN@�1'@���@��@��D@�|�@�~�@�$�@�ff@��T@�X@�G�@�&�@��D@���@�\)@�;d@��y@��!@���@���@�v�@�M�@��h@�`B@�O�@�x�@��@�M�@�M�@���@���@��@�v�@�V@�-@��@��h@�?}@���@��u@���@��@�@�v�@���@���@��^@���@���@{�@k�011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
L�B
M�B
L�B
L�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
Q�B
VB
\)B
p�B
�{B
�LB
��B
�B
�B
�5B
�;B
�;B
�;B
�;B
�HB
�NB
�NB
�HB
�NB
�NB
�TB
�ZB
�TB
�;B
�5B
�;B
�ZB
�mB
�sB
�yB
�B
��BB\BJBVB$�B49BD�BK�B\)B� B��B��B��B�wB�`B�BB%B1B1B\B�BF�BR�B[#Bs�B�7B��B�LB�qB��B��B��B��B�qB�-B��B��B�Bv�B]/B5?B��B�B�`BĜB�7B]/B@�B5?B,B%�B�B
�TB
��B
�^B
�=B
q�B
ZB
,B
%�B
�B
oB
\B
B	�NB	ȴB	�RB	�!B	��B	��B	�7B	{�B	hsB	ZB	L�B	L�B	I�B	G�B	E�B	?}B	49B	#�B	�B	�B	{B	PB��B�B�B�yB�fB�NB�5B�#B�B�B�B��B��B��B��B��B��BɺBȴBƨBƨBĜBŢB��B��B��B��B��B��B�#B�BB�HB�5B�B�;B�HB�;B�B��BȴB��B�}B�}B�RB�9B�B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�RBƨB��B�B�B��B��B��BBÖBɺB��B�B�NB�B�HB��B�
B�B	oB	,B	/B	2-B	/B	,B	%�B	.B	,B	 �B	,B	1'B	2-B	-B	)�B	"�B	!�B	#�B	"�B	�B	�B	hB	oB	�B	�B	7LB	P�B	Q�B	P�B	N�B	K�B	F�B	G�B	I�B	M�B	L�B	F�B	@�B	7LB	%�B	�B��B�B��B��B��B��B��B��B��B��B	B	
=B	DB	�B	�B	 �B	%�B	+B	)�B	+B	33B	;dB	?}B	B�B	B�B	>wB	=qB	?}B	?}B	@�B	E�B	K�B	M�B	L�B	L�B	N�B	T�B	_;B	`BB	_;B	^5B	cTB	hsB	iyB	jB	p�B	s�B	q�B	o�B	l�B	k�B	n�B	l�B	k�B	k�B	m�B	l�B	k�B	l�B	m�B	o�B	r�B	s�B	u�B	z�B	|�B	�B	�B	�B	�7B	�JB	�\B	�bB	�VB	�JB	�=B	�JB	�bB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	��B
	�B
)B
+Q22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
L�B
M�B
L�B
L�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
Q�B
VB
\)B
p�B
�{B
�LB
��B
�B
�B
�5B
�;B
�;B
�;B
�;B
�HB
�NB
�NB
�HB
�NB
�NB
�TB
�ZB
�TB
�;B
�5B
�;B
�ZB
�mB
�sB
�yB
�B
��BB\BJBVB$�B49BD�BK�B\)B� B��B��B��B�wB�`B�BB%B1B1B\B�BF�BR�B[#Bs�B�7B��B�LB�qB��B��B��B��B�qB�-B��B��B�Bv�B]/B5?B��B�B�`BĜB�7B]/B@�B5?B,B%�B�B
�TB
��B
�^B
�=B
q�B
ZB
,B
%�B
�B
oB
\B
B	�NB	ȴB	�RB	�!B	��B	��B	�7B	{�B	hsB	ZB	L�B	L�B	I�B	G�B	E�B	?}B	49B	#�B	�B	�B	{B	PB��B�B�B�yB�fB�NB�5B�#B�B�B�B��B��B��B��B��B��BɺBȴBƨBƨBĜBŢB��B��B��B��B��B��B�#B�BB�HB�5B�B�;B�HB�;B�B��BȴB��B�}B�}B�RB�9B�B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�RBƨB��B�B�B��B��B��BBÖBɺB��B�B�NB�B�HB��B�
B�B	oB	,B	/B	2-B	/B	,B	%�B	.B	,B	 �B	,B	1'B	2-B	-B	)�B	"�B	!�B	#�B	"�B	�B	�B	hB	oB	�B	�B	7LB	P�B	Q�B	P�B	N�B	K�B	F�B	G�B	I�B	M�B	L�B	F�B	@�B	7LB	%�B	�B��B�B��B��B��B��B��B��B��B��B	B	
=B	DB	�B	�B	 �B	%�B	+B	)�B	+B	33B	;dB	?}B	B�B	B�B	>wB	=qB	?}B	?}B	@�B	E�B	K�B	M�B	L�B	L�B	N�B	T�B	_;B	`BB	_;B	^5B	cTB	hsB	iyB	jB	p�B	s�B	q�B	o�B	l�B	k�B	n�B	l�B	k�B	k�B	m�B	l�B	k�B	l�B	m�B	o�B	r�B	s�B	u�B	z�B	|�B	�B	�B	�B	�7B	�JB	�\B	�bB	�VB	�JB	�=B	�JB	�bB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	��B
	�B
)B
+Q22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190515                              AO  ARCAADJP                                                                    20181005190515    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190515  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181005190515  QCF$                G�O�G�O�G�O�8000            