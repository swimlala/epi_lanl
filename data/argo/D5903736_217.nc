CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-04-24T00:02:09Z creation      
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180424000209  20190604094145  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�]w˩�1   @�]xlw��@6%�S����dƧ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A!��AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�{D�C�D��fD��)D� D�P D���D���D�3D�2�D��D��D��
D�*�D�ffD��\D���D�7�D�f�D�t)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @xQ�@��\@�\A�HA:�HAYG�AyG�A���A���A���A���Ạ�Aܣ�A��A���BQ�BQ�BQ�BQ�B&�RB.Q�B6Q�B>Q�BFQ�BNQ�BVQ�B^Q�BfQ�BnQ�BvQ�B~Q�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D eD �DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�D	eD	�D
eD
�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�D eD �D!eD!�D"eD"�D#eD#�D$eD$�D%eD%�D&eD&�D'eD'�D(eD(�D)eD)�D*eD*�D+eD+�D,eD,�D-eD-�D.eD.�D/eD/�D0eD0�D1eD1�D2eD2�D3eD3�D4eD4�D5eD5�D6eD6�D7eD7�D8eD8�D9eD9�D:eD:�D;eD;�D<eD<�D=eD=޹D>eD>�D?eD?�D@eD@�DAeDA�DBeDB�DCeDC�DDeDD�DEeDE�DFeDF�DGeDG�DHeDH�DIeDI�DJeDJ�DKeDK�DLeDL�DMeDM�DNeDN�DOeDO�DPk�DP�DQeDQ�DReDR�DSeDS�DTeDT�DUeDU�DVeDV�DWeDW�DXeDX�DYeDY�DZeDZ�D[eD[�D\eD\�D]eD]�D^eD^�D_eD_�D`eD`�DaeDa�DbeDb�DceDc�DdeDd�DeeDe�DfeDf�DgeDg�DheDh�DieDi�DjeDj�DkeDk�DleDl�DmeDm�DneDn�DoeDo�DpeDp�DqeDq�DreDr�DseDs�DteDt�Dy|�D�
D�6D���D���D���D�B�D��D��D��D�%D���D���D��D�pD�X�D���D��RD�*=D�YGD�f�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AƼjAƼjAƸRAƩ�AƬAƬAƬAƬAƬAƮAƬAƮAƩ�AƬAƬAƬAƬAƮAƮAƮAư!Aư!AƲ-AƲ-Aƴ9Aư!AƓuA�$�A�XA�  A�VA�t�A���A�C�A�/A���A�+A��#A���A�~�A��-A�=qA��
A���A���A�A�VA�bA�n�A��A�A��wA�S�A��wA��TA��FA���A���A��RA�ĜA�%A��
A�G�A�jA���A�oA�1A�G�A�A�XA���A��
A��A�l�A��A�A��#A�5?A���A��\A��A��A��uA�7LA��`A�S�A�A�A���A��A�S�A��A�$�A��#A�t�A��
A�ȴA���A�dZA�ĜA�n�A�33A���A�1A���A�O�A���A�C�A��A��A~��A|~�Az��Aw�#AuK�At{ArVAq�Aq��Aq&�Am�Ajr�Ai�wAi��Ai%Ah �AfA�Aa�^A]�FA\5?AZjAXM�AW&�AV5?AT�uASAP��AP5?AOVAM�AL$�AJĜAH�HAG+ACO�AB �A@�A@v�A?�;A>M�A<��A<�A;�-A:�jA8�A6�RA3��A2��A2E�A1\)A/?}A,��A+�7A*�A)ƨA(ffA&�uA%�-A#�-A"ffA!p�A n�A��A;dA��A�At�A�jA��A�A�
A7LA=qA��A7LAz�A�A~�A��AXA�AQ�A  A�A�-A�DA�
A;dA
ĜA
n�A	�A	�A	&�Az�A�FA33A�+A1A��A�PA|�Al�A��A�A(�A�A�A��AS�A �D@��\@�b@�J@�(�@�ȴ@�p�@��j@�9X@��@���@��@@�33@�+@��#@�9X@�w@�P@�~�@�O�@�1'@��@��@�33@��@�J@�\)@�ff@߮@���@ܓu@ڟ�@ى7@�7L@��@�z�@׍P@��y@�-@�X@�j@Ӯ@��y@ѡ�@ϥ�@�+@·+@�A�@�S�@���@��@���@�33@��y@���@��@�p�@��`@�9X@�|�@��@�n�@�O�@�j@���@�@��!@���@�=q@�hs@��@��@��@�C�@�o@��!@�@��^@��@��j@�Ĝ@��/@�V@�hs@�7L@���@�r�@��F@�V@�X@���@��@��@��D@�(�@���@�@�?}@��j@�r�@���@�;d@�E�@���@�7L@�Ĝ@�r�@��;@��@�E�@�`B@��@��9@�Q�@� �@��@��@��@�dZ@���@�=q@���@�`B@��/@���@�(�@�ƨ@�S�@���@�5?@�@���@��h@�X@�&�@��`@��u@�A�@�(�@�b@���@��@�
=@��y@���@���@�~�@�M�@�-@��T@���@�p�@�7L@��/@��u@�r�@�bN@�I�@�1@��@��@��P@�l�@�S�@�;d@��@��H@���@���@�v�@�-@�@��-@�V@��/@�Ĝ@��@�j@�1'@�  @��@���@��@�+@��H@��\@�M�@��@���@��7@�`B@�&�@��9@��@�bN@�b@��m@��
@���@�\)@�"�@�@��R@���@�~�@�v�@�V@�J@���@�x�@�X@�&�@�V@��`@��@���@�j@�1'@��@��;@���@�ƨ@��@���@��@�S�@�+@�@��H@���@��+@�v�@�V@�-@�@���@��-@�p�@�?}@��@�Ĝ@��D@�r�@�I�@�1@��m@���@��w@��F@��F@��P@�33@��y@���@���@�~�@�^5@�M�@�$�@��@�@���@��@�`B@�7L@�%@���@��@�Z@��P@���@���@w�g@rȴ@l7�@e�@Z�y@Rz@Ic�@C�@=!�@8��@3)_@.�r@)^�@#]�@X@҉@@�@@@
R�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AƼjAƼjAƸRAƩ�AƬAƬAƬAƬAƬAƮAƬAƮAƩ�AƬAƬAƬAƬAƮAƮAƮAư!Aư!AƲ-AƲ-Aƴ9Aư!AƓuA�$�A�XA�  A�VA�t�A���A�C�A�/A���A�+A��#A���A�~�A��-A�=qA��
A���A���A�A�VA�bA�n�A��A�A��wA�S�A��wA��TA��FA���A���A��RA�ĜA�%A��
A�G�A�jA���A�oA�1A�G�A�A�XA���A��
A��A�l�A��A�A��#A�5?A���A��\A��A��A��uA�7LA��`A�S�A�A�A���A��A�S�A��A�$�A��#A�t�A��
A�ȴA���A�dZA�ĜA�n�A�33A���A�1A���A�O�A���A�C�A��A��A~��A|~�Az��Aw�#AuK�At{ArVAq�Aq��Aq&�Am�Ajr�Ai�wAi��Ai%Ah �AfA�Aa�^A]�FA\5?AZjAXM�AW&�AV5?AT�uASAP��AP5?AOVAM�AL$�AJĜAH�HAG+ACO�AB �A@�A@v�A?�;A>M�A<��A<�A;�-A:�jA8�A6�RA3��A2��A2E�A1\)A/?}A,��A+�7A*�A)ƨA(ffA&�uA%�-A#�-A"ffA!p�A n�A��A;dA��A�At�A�jA��A�A�
A7LA=qA��A7LAz�A�A~�A��AXA�AQ�A  A�A�-A�DA�
A;dA
ĜA
n�A	�A	�A	&�Az�A�FA33A�+A1A��A�PA|�Al�A��A�A(�A�A�A��AS�A �D@��\@�b@�J@�(�@�ȴ@�p�@��j@�9X@��@���@��@@�33@�+@��#@�9X@�w@�P@�~�@�O�@�1'@��@��@�33@��@�J@�\)@�ff@߮@���@ܓu@ڟ�@ى7@�7L@��@�z�@׍P@��y@�-@�X@�j@Ӯ@��y@ѡ�@ϥ�@�+@·+@�A�@�S�@���@��@���@�33@��y@���@��@�p�@��`@�9X@�|�@��@�n�@�O�@�j@���@�@��!@���@�=q@�hs@��@��@��@�C�@�o@��!@�@��^@��@��j@�Ĝ@��/@�V@�hs@�7L@���@�r�@��F@�V@�X@���@��@��@��D@�(�@���@�@�?}@��j@�r�@���@�;d@�E�@���@�7L@�Ĝ@�r�@��;@��@�E�@�`B@��@��9@�Q�@� �@��@��@��@�dZ@���@�=q@���@�`B@��/@���@�(�@�ƨ@�S�@���@�5?@�@���@��h@�X@�&�@��`@��u@�A�@�(�@�b@���@��@�
=@��y@���@���@�~�@�M�@�-@��T@���@�p�@�7L@��/@��u@�r�@�bN@�I�@�1@��@��@��P@�l�@�S�@�;d@��@��H@���@���@�v�@�-@�@��-@�V@��/@�Ĝ@��@�j@�1'@�  @��@���@��@�+@��H@��\@�M�@��@���@��7@�`B@�&�@��9@��@�bN@�b@��m@��
@���@�\)@�"�@�@��R@���@�~�@�v�@�V@�J@���@�x�@�X@�&�@�V@��`@��@���@�j@�1'@��@��;@���@�ƨ@��@���@��@�S�@�+@�@��H@���@��+@�v�@�V@�-@�@���@��-@�p�@�?}@��@�Ĝ@��D@�r�@�I�@�1@��m@���@��w@��F@��F@��P@�33@��y@���@���@�~�@�^5@�M�@�$�@��@�@���@��@�`B@�7L@�%@���@��@�ZG�O�@���@���@w�g@rȴ@l7�@e�@Z�y@Rz@Ic�@C�@=!�@8��@3)_@.�r@)^�@#]�@X@҉@@�@@@
R�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�=B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�JB
��B
�dB
��BZB��B��B��BB"�B&�B9XB9XB=qB<jBD�B^5BjBw�B~�B}�B� B�%B�%B�B�B�B� B� B{�Bz�Bx�Bu�Bs�Bo�Bk�BbNBQ�BE�B;dB5?B+B�B�B�BbB+B��B�B�sB�sB�`B�/B�B��B��B�^B�9B�B��B��B}�Bm�BbNBVBJ�B,B�BoB
��B
�B
�mB
�HB
�B
ɺB
�jB
�B
��B
�uB
�\B
�1B
� B
v�B
k�B
O�B
?}B
33B
�B
VB
B	��B	��B	��B	�B	�B	ɺB	ÖB	B	�qB	�FB	��B	�JB	v�B	l�B	bNB	W
B	O�B	I�B	@�B	8RB	.B	)�B	#�B	�B	uB	DB	B��B�fB�BB�B�
B��B��BǮBŢBÖB��B�jB�RB�?B�-B�'B�B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�VB�PB�JB�DB�7B�1B�B�B}�B{�Bz�By�Bx�Bv�Bu�Bs�Bs�Br�Bq�Bp�Bn�Bm�Bk�Bk�BjBiyBiyBhsBhsBhsBhsBhsBhsBgmBhsBgmBhsBhsBhsBgmBgmBgmBffBffBffBffBe`BdZBcTBbNBbNBffBiyBhsBgmBgmBffBhsBl�Bs�Bt�Bu�Bs�Bq�Br�Bs�Bu�Bv�Bw�Bv�Bv�Bv�Bv�Bw�Bz�Bz�B{�B�B�+B�7B�DB�JB�PB�PB�JB�JB�JB�PB�\B�\B�\B�hB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�9B�FB�LB�dB�wB��BÖBĜBŢBǮBȴB��B��B��B��B��B��B��B��B�B�B�)B�/B�5B�NB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	DB	\B	oB	{B	�B	�B	 �B	%�B	-B	/B	2-B	5?B	6FB	8RB	:^B	;dB	<jB	@�B	D�B	G�B	J�B	N�B	P�B	S�B	W
B	ZB	^5B	bNB	dZB	ffB	hsB	jB	k�B	m�B	o�B	q�B	r�B	s�B	s�B	v�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�+B	�1B	�DB	�VB	�\B	�\B	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�RB	�XB	�dB	�jB	�wB	�}B	��B	ÖB	ĜB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
�B
�B
B
B
(�B
/�B
6�B
AB
G�B
N�B
S�B
W�B
\�B
a�B
d�B
gB
n�B
t�B
zDB
}<B
��B
�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
zqB
ypB
zrB
zqB
zrB
zvB
zsB
zpB
zsB
zvB
zvB
zvB
zsB
yoB
zsB
ztB
zqB
zsB
ztB
zsB
zpB
zsB
zqB
ztB
zsB
|�B
��B
��B
��BJ<B�B��B��B�B�B�B)^B)^B-yB,pB4�BN9BZBg�Bn�Bm�Bo�Bv&Bv%BrBrBsBo�BpBk�Bj�Bh�Be�Bc�B_�B[�BRUBA�B5�B+vB%QBB�B
�B�B uB�AB��BܧB؎B؋B�}B�MB�*B��B��B��B�\B�8B��B��Bn%B]�BR�BF:B:�B@B
�B�B
�9B
��B
׳B
юB
�VB
� B
��B
�RB
��B
��B
�B
x�B
pXB
gB
[�B
@9B
/�B
#�B
B	��B	��B	�CB	�5B	�,B	�B	ʏB	�+B	�B	� B	��B	��B	�fB	|�B	gHB	]B	R�B	G�B	@_B	:>B	1B	(�B	�B	�B	dB	*B	B��B�B�aB��B��BʲBǡBÆB�`B�GB�8B�/B�%B�B��B��B��B��B��B��B��B�wB�oB�eB�OB�?B�2B�B�B�B�BB}�B|�B{�By�Bx�Bu�Bq�Bn�Bl�Bk�Bj�Bi�BgtBfnBdeBdfBc]BbXBaSB_FB^@B\3B\6B[,BZ)BZ*BY%BY$BY%BY#BY$BY"BXBY(BX!BY(BY'BY(BXBXBX"BWBWBWBWBVBUBTBSBSBWBZ.BY*BX!BX%BWBY*B]ABdiBeqBfyBdkBb`BcdBdjBfxBg~Bh�Bg�Bg~Bg�Bg�Bh�Bk�Bk�Bl�Bs�Bw�By�B{�B}B~B~B|�B|�B}B~B�B�B�B�B�=B�BB�@B�gB�nB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�(B�6B�IB�OB�WB�^B�dB�{B��B��B��B��B��BĪBĩBưB��B��B��B��B��B�$B�/B�4B�6B�FB�NB�HB�DB�IB�OB�YB�jB�zB�B�B�B�B��B��B	 B	B	&B	4B	RB	mB	�B	�B	�B	"�B	%�B	&�B	(�B	+B	,B	-B	1,B	5DB	8VB	;jB	?~B	A�B	D�B	G�B	J�B	N�B	R�B	T�B	WB	YB	["B	\*B	^3B	`BB	bOB	cSB	dZB	d[B	giB	k�B	l�B	m�B	n�B	o�B	q�B	r�B	t�B	u�B	w�B	x�B	{�B	~�B	�B	�B	� B	�B	�B	�!B	� B	�(B	�,B	�3B	�9B	�CB	�LB	�IB	�OB	�eB	�hB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�.B	�8B	�8B	�DB	�HB	�OB	�XB	�`B	�dB	�kB	�vB	�}B	�~B	B	B	čB	ƚB	ǣB	ȪB	ɮB	ɭB	ʷB	˻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�"B	�.B	�6B	�4B	�@B	�=B	�FB	�KB	�RB	�QB	�XB	�XB	�WB	�aB	�`B	�iB	�rB	�qB	�tB	�uB	�{B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�G�O�B	�B
 �B
	�B
RB
 B
'�B
1�B
8�B
?MB
D7B
H�B
M:B
RxB
UpB
W�B
_[B
eaB
j�B
m�B
qzB
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.42 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941452019060409414520190604094145  AO  ARCAADJP                                                                    20180424000209    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180424000209  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180424000209  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094145  IP                  G�O�G�O�G�O�                