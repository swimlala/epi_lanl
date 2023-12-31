CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-12-26T08:02:41Z creation; 2023-02-10T23:09:45Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    U�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  [�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    t   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  z8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x  �X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x %�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` >    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   >`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   D`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   J`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T P`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   P�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   P�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   P�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   P�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � P�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   QT   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Qp   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Qx   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        Q�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        Q�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       Q�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    Q�Argo profile    3.1 1.2 19500101000000  20221226080241  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_242                 6810_008521_242                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�Q���@�Q���11  @�R-V@�R-V@2p�N��@2p�N���d�z�N���d�z�N��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�@   @@  @}p�@�  @�  @�G�@�p�A�RA\)A+�A@  A_\)A\)A�  A�  A�Q�A�  AϮA߮A�  B   B  B  B�
B�
B'�
B0  B7�
B?�BG�
BP  BX(�B_�Bg�Bp  BxQ�B�=qB�=qB�{B��B��B��B�  B�{B�{B��B�{B�(�B�{B�  B�(�B�{B��
B�  B�  B�  B�(�B�(�B�{B�  B��B��B�  B�  B�  B�  B�  B�{C {C
=C  C  C  C	��C�C�C��C  C  C  C��C  C
=C  C��C"  C$  C&
=C(
=C*
=C,
=C.
=C0  C2
=C4  C6  C8
=C:  C<  C=��C?��CB  CD  CF
=CH
=CI��CL  CM��CO��CR  CT
=CU��CW��CY��C[��C]�C`  Cb{Cd
=Ce��Ch
=Cj
=Cl  Cm��Cp  Cr  Ct  Cu��Cx  Cy�C{��C~  C�  C�C�  C���C���C���C�  C�C���C�  C�
=C�C�  C�  C�  C�  C�  C�C�  C�  C�C�C�  C�  C���C���C�C�  C�C�  C���C�  C�
=C�
=C�  C�  C�
=C�C���C���C�C�C���C���C�  C�  C�  C�  C�C�  C�C�C���C���C�  C�C�C�
=C�  C���C���C�  C�  C�  C�C�  C�C�
=C�C�  C�C���C���C�  C�  C���C�  C�  C�  C�  C�  C�  C���C�  C�  C���C�  C�  C���C�  C�
=C�
=C�
=C�
=C�C�C�C�C�
=C�C�
=C�  C���C�C�
=C�C���C�  C���C�  C�  C���C�  C�  C�
=C�C�C�C�
=C���C���C�  C�  C�C�C�  C���C�  D �D � D �RD}qD  D� DD��D�D� D�D��D  D� D  D� D  D�D	  D	xRD
  D
��D�D�D�D��D�D}qD�qD}qD  D� D�D� D  D��DD��D�D}qD��D}qD  D}qD�qDxRD��D� D�D� D�qD��D  D� D�D}qD�qD}qD��D}qD�qD� D�D� D �D � D �qD!}qD!�qD"� D#�D#� D$�D$��D%�D%}qD%�qD&� D'�D'��D(�D(��D)�D)��D*�D*��D+  D+�D,D,��D-  D-z�D-��D.}qD.�qD/}qD/�qD0� D1�D1� D1��D2z�D2�qD3� D3�qD4}qD5  D5� D6�D6� D6��D7}qD8�D8��D9D9}qD9�qD:��D;  D;� D<  D<}qD=  D=��D>  D>� D?�D?� D?��D@}qD@�qDA}qDB  DB� DC  DC}qDD  DD� DE�DE�DF�DF� DG�DG� DG�qDH� DH�qDI� DJ  DJ� DK�DK� DK�qDL� DM  DM� DN  DN}qDN��DO}qDP  DP� DP�qDQ}qDRDR��DSDS��DT  DT� DT��DU}qDU�qDV� DW  DW� DX  DX��DY�DY��DY�qDZ}qDZ�qD[}qD[��D\z�D\�qD]� D^�D^��D^�qD_� D`D`��Da  Da}qDa�qDb}qDb�qDc� Dd  Dd� De  De}qDf  Df� Dg  Dg��Dh  Dh}qDi  Di� Dj  Dj}qDj�qDk� Dl  Dl��Dm�Dm}qDm�qDn� Do  Do� Do�qDp}qDp�qDq��Dr�Dr}qDr�qDs� Ds�qDt� Dt�qDu� Dv�Dv� Dw�Dw��Dx  Dx��Dy�Dy� Dy�qDz� D{  D{}qD|  D|� D|�qD}� D~  D~}qD~�qD� D�  D�>�D�~�D���D���D�@ D��HD��HD���D�>�D��HD��HD��D�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�� D���D��qD�>�D�~�D�� D�  D�>�D�� D�� D���D�>�D�� D�� D���D�@ D�� D���D�  D�@ D�~�D���D���D�@ D��HD��HD�HD�'�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?�?B�\?�=q?�{?�G�@�\@�R@5@G�@c�
@s33@�=q@���@�p�@���@���@��R@�ff@�z�@޸R@�@�z�@��HAz�A	��A�RA�
AQ�A�RA!�A(Q�A+�A2�\A6ffA:�HAAG�ADz�AK�AP��AU�A\(�A_\)AfffAj�HAp��Aw
=Az�HA�  A��A�A���A�(�A�ffA���A���A�ffA��A�z�A�
=A�=qA�z�A��A�=qA�(�A�  A��A�z�A��A��A���A��A�G�A���A�\)A�G�A�z�A�\)A�G�A�z�A�\)A�G�A�z�A�\)A�G�A���A�RA��A��A�
=A��A��A�
=A�=qA��A�
=BG�BffB�
BG�BffB(�B	G�B
�\BQ�Bp�B�HBz�B��B
=B��B��B33B��BB\)B��B{B33B ��B"=qB#\)B%�B&{B'\)B)�B)�B+�B-�B.{B/�B1�B2=qB3�
B5�B6=qB8  B9G�B:ffB<(�B=G�B>ffB@(�BAG�BB�HBDQ�BEG�BF�HBHQ�BI��BJ�RBLz�BM��BN�HBPz�BQ��BR�HBT��BUp�BW33BXz�BY��B[\)B\z�B]��B_33B`z�Bap�Bc33Bdz�Be��Bg\)Bhz�Bi��Bk�Blz�BmBo�Bp��Bq�Bs�Btz�Bv=qBw33Bxz�Bz=qB{�B|��B~=qB\)B�Q�B�33B��B�ffB��B���B�ffB��B��B�Q�B�33B�B�z�B�33B��B�z�B�33B�B�ffB��B��B�z�B��B���B�z�B���B�B�ffB��HB��B�Q�B���B���B�Q�B��RB��B�(�B���B��B�{B���B��B�  B��RB��B�  B���B��B�  B��RB�p�B�  B��HB�p�B�{B��HB�\)B�Q�B���B��B�Q�B���B��B�ffB�
=B��B�ffB���B���B�ffB��B���B�Q�B��B�B�=qB�
=B�B�=qB�
=B��B�=qB��B��B�=qB��B��B�Q�B��B��B�ffB��B���B�z�B�
=B���B�ffB�
=B���B�z�B���BîB�z�B�
=BŅB�ffB��BǙ�B�Q�B��Bə�B�ffB�
=B˙�B�ffB��B͙�B�z�B��Bϙ�B�ffB���BхB�ffB���B�p�B�=qB��HB�\)B��BָRB��B׮B�Q�BظRB�33B��
B�(�Bڣ�B�G�BۅB�{B܏\B���B�G�BݮB�  Bޏ\B���B��B߮B��
B�(�B�RB�
=B�G�B�B�{B�Q�B��HB��B�p�B��
B�=qB�ffB���B�\)B�p�B��B�ffB�z�B�
=B�\)B癚B�  B�z�B�RB���B�p�B��
B�  B�ffB��HB��B�\)B��B�=qB�ffB��HB�G�B홚B��
B�Q�B�\B�RB�G�B�B��
B�=qB��\B�RB�33B�B�B�{B�\B��HB�
=B�B��B�{B�Q�B��HB��B�\)B��B�(�B�ffB���B��B�p�B���B�{B�ffB��\B�
=B�p�B��B��
B�ffB��\B���B�G�B���B�B�{B��\B��RB��B��B��B�  B�z�B��RB���B��B�B��C =qC ffC z�C �RC �HC  C33CffCp�C�C��C�C�CG�C\)C��C�RC�
C
=CG�Cp�C�CC�C  CG�CQ�C�\CC��C{C(�CG�C�C�CC��C33CG�Cp�C�C�
C��C(�C\)Cz�C��C�C  C=qCp�C�C�RC	  C	{C	=qC	�C	��C	��C

=C
�C
\)C
�\C
�C
�HC�C33Cz�C��C�RC
=C(�CQ�C��C�C�HC(�CQ�Cp�CC�HC{CffC�CC  C�Cp�C��CC{C33CffC�C�
C
=CG�C�C�RC�
C{C\)Cz�C�C�C(�CG�Cz�C��C��C�CffC�C��C��C=qCp�C��C��C
=C=qCffC�C�C�C=qC�\CC�C�Cp�C�C�
C  CQ�C��C��C��C=qC�C�C�C33C\)C�\C�HC�CG�C�C��C  C(�Cp�CC�C�C\)C�C�C�CG�C��C��C��C Q�C �C �RC ��C!=qC!p�C!��C!�HC"33C"ffC"�\C"�
C#�C#\)C#�C#�RC$  C$Q�C$�\C$�C$��C%=qC%p�C%��C%�HC&33C&p�C&��C&��C'�C'\)C'��C'��C(  C(Q�C(�\C(�C(��C)=qC)p�C)��C)��C*(�C*\)C*�\C*��C+�C+ffC+��C+C+��C,G�C,�\C,�RC,�C-(�C-z�C-�RC-�C.{C.G�C.�\C.��C.��C/�C/ffC/��C/�C0
=C0=qC0z�C0C1  C133C1ffC1��C1�HC2(�C2G�C2z�C2C2��C3(�C3ffC3�C3�
C4  C4G�C4�\C4C4�HC533C5z�C5��C5��C6{C6\)C6�\C6�RC6�C733C7z�C7�C7�HC8{C8=qC8z�C8C8�C9�C9\)C9��C9�
C:  C:33C:ffC:��C:�
C;{C;Q�C;z�C;��C;�HC<(�C<ffC<�\C<�C<�HC=(�C=ffC=�\C=�RC=��C>=qC>p�C>��C>C?  C?=qC?z�C?��C?C@
=C@G�C@z�C@��C@��CA
=CAG�CAz�CA��CA�
CB
=CBG�CB�\CBCB�CC{CCG�CC�\CC��CC��CD{CD\)CD��CD�
CD��CE�CEffCE��CE�
CF  CF(�CFffCF�RCF�HCG{CG=qCGz�CG�RCH  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                        111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?k�@   @@  @}p�@�  @�  @�G�@�p�A�RA\)A+�A@  A_\)A\)A�  A�  A�Q�A�  AϮA߮A�  B   B  B  B�
B�
B'�
B0  B7�
B?�BG�
BP  BX(�B_�Bg�Bp  BxQ�B�=qB�=qB�{B��B��B��B�  B�{B�{B��B�{B�(�B�{B�  B�(�B�{B��
B�  B�  B�  B�(�B�(�B�{B�  B��B��B�  B�  B�  B�  B�  B�{C {C
=C  C  C  C	��C�C�C��C  C  C  C��C  C
=C  C��C"  C$  C&
=C(
=C*
=C,
=C.
=C0  C2
=C4  C6  C8
=C:  C<  C=��C?��CB  CD  CF
=CH
=CI��CL  CM��CO��CR  CT
=CU��CW��CY��C[��C]�C`  Cb{Cd
=Ce��Ch
=Cj
=Cl  Cm��Cp  Cr  Ct  Cu��Cx  Cy�C{��C~  C�  C�C�  C���C���C���C�  C�C���C�  C�
=C�C�  C�  C�  C�  C�  C�C�  C�  C�C�C�  C�  C���C���C�C�  C�C�  C���C�  C�
=C�
=C�  C�  C�
=C�C���C���C�C�C���C���C�  C�  C�  C�  C�C�  C�C�C���C���C�  C�C�C�
=C�  C���C���C�  C�  C�  C�C�  C�C�
=C�C�  C�C���C���C�  C�  C���C�  C�  C�  C�  C�  C�  C���C�  C�  C���C�  C�  C���C�  C�
=C�
=C�
=C�
=C�C�C�C�C�
=C�C�
=C�  C���C�C�
=C�C���C�  C���C�  C�  C���C�  C�  C�
=C�C�C�C�
=C���C���C�  C�  C�C�C�  C���C�  D �D � D �RD}qD  D� DD��D�D� D�D��D  D� D  D� D  D�D	  D	xRD
  D
��D�D�D�D��D�D}qD�qD}qD  D� D�D� D  D��DD��D�D}qD��D}qD  D}qD�qDxRD��D� D�D� D�qD��D  D� D�D}qD�qD}qD��D}qD�qD� D�D� D �D � D �qD!}qD!�qD"� D#�D#� D$�D$��D%�D%}qD%�qD&� D'�D'��D(�D(��D)�D)��D*�D*��D+  D+�D,D,��D-  D-z�D-��D.}qD.�qD/}qD/�qD0� D1�D1� D1��D2z�D2�qD3� D3�qD4}qD5  D5� D6�D6� D6��D7}qD8�D8��D9D9}qD9�qD:��D;  D;� D<  D<}qD=  D=��D>  D>� D?�D?� D?��D@}qD@�qDA}qDB  DB� DC  DC}qDD  DD� DE�DE�DF�DF� DG�DG� DG�qDH� DH�qDI� DJ  DJ� DK�DK� DK�qDL� DM  DM� DN  DN}qDN��DO}qDP  DP� DP�qDQ}qDRDR��DSDS��DT  DT� DT��DU}qDU�qDV� DW  DW� DX  DX��DY�DY��DY�qDZ}qDZ�qD[}qD[��D\z�D\�qD]� D^�D^��D^�qD_� D`D`��Da  Da}qDa�qDb}qDb�qDc� Dd  Dd� De  De}qDf  Df� Dg  Dg��Dh  Dh}qDi  Di� Dj  Dj}qDj�qDk� Dl  Dl��Dm�Dm}qDm�qDn� Do  Do� Do�qDp}qDp�qDq��Dr�Dr}qDr�qDs� Ds�qDt� Dt�qDu� Dv�Dv� Dw�Dw��Dx  Dx��Dy�Dy� Dy�qDz� D{  D{}qD|  D|� D|�qD}� D~  D~}qD~�qD� D�  D�>�D�~�D���D���D�@ D��HD��HD���D�>�D��HD��HD��D�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�� D���D��qD�>�D�~�D�� D�  D�>�D�� D�� D���D�>�D�� D�� D���D�@ D�� D���D�  D�@ D�~�D���D���D�@ D��HD��HD�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?�?B�\?�=q?�{?�G�@�\@�R@5@G�@c�
@s33@�=q@���@�p�@���@���@��R@�ff@�z�@޸R@�@�z�@��HAz�A	��A�RA�
AQ�A�RA!�A(Q�A+�A2�\A6ffA:�HAAG�ADz�AK�AP��AU�A\(�A_\)AfffAj�HAp��Aw
=Az�HA�  A��A�A���A�(�A�ffA���A���A�ffA��A�z�A�
=A�=qA�z�A��A�=qA�(�A�  A��A�z�A��A��A���A��A�G�A���A�\)A�G�A�z�A�\)A�G�A�z�A�\)A�G�A�z�A�\)A�G�A���A�RA��A��A�
=A��A��A�
=A�=qA��A�
=BG�BffB�
BG�BffB(�B	G�B
�\BQ�Bp�B�HBz�B��B
=B��B��B33B��BB\)B��B{B33B ��B"=qB#\)B%�B&{B'\)B)�B)�B+�B-�B.{B/�B1�B2=qB3�
B5�B6=qB8  B9G�B:ffB<(�B=G�B>ffB@(�BAG�BB�HBDQ�BEG�BF�HBHQ�BI��BJ�RBLz�BM��BN�HBPz�BQ��BR�HBT��BUp�BW33BXz�BY��B[\)B\z�B]��B_33B`z�Bap�Bc33Bdz�Be��Bg\)Bhz�Bi��Bk�Blz�BmBo�Bp��Bq�Bs�Btz�Bv=qBw33Bxz�Bz=qB{�B|��B~=qB\)B�Q�B�33B��B�ffB��B���B�ffB��B��B�Q�B�33B�B�z�B�33B��B�z�B�33B�B�ffB��B��B�z�B��B���B�z�B���B�B�ffB��HB��B�Q�B���B���B�Q�B��RB��B�(�B���B��B�{B���B��B�  B��RB��B�  B���B��B�  B��RB�p�B�  B��HB�p�B�{B��HB�\)B�Q�B���B��B�Q�B���B��B�ffB�
=B��B�ffB���B���B�ffB��B���B�Q�B��B�B�=qB�
=B�B�=qB�
=B��B�=qB��B��B�=qB��B��B�Q�B��B��B�ffB��B���B�z�B�
=B���B�ffB�
=B���B�z�B���BîB�z�B�
=BŅB�ffB��BǙ�B�Q�B��Bə�B�ffB�
=B˙�B�ffB��B͙�B�z�B��Bϙ�B�ffB���BхB�ffB���B�p�B�=qB��HB�\)B��BָRB��B׮B�Q�BظRB�33B��
B�(�Bڣ�B�G�BۅB�{B܏\B���B�G�BݮB�  Bޏ\B���B��B߮B��
B�(�B�RB�
=B�G�B�B�{B�Q�B��HB��B�p�B��
B�=qB�ffB���B�\)B�p�B��B�ffB�z�B�
=B�\)B癚B�  B�z�B�RB���B�p�B��
B�  B�ffB��HB��B�\)B��B�=qB�ffB��HB�G�B홚B��
B�Q�B�\B�RB�G�B�B��
B�=qB��\B�RB�33B�B�B�{B�\B��HB�
=B�B��B�{B�Q�B��HB��B�\)B��B�(�B�ffB���B��B�p�B���B�{B�ffB��\B�
=B�p�B��B��
B�ffB��\B���B�G�B���B�B�{B��\B��RB��B��B��B�  B�z�B��RB���B��B�B��C =qC ffC z�C �RC �HC  C33CffCp�C�C��C�C�CG�C\)C��C�RC�
C
=CG�Cp�C�CC�C  CG�CQ�C�\CC��C{C(�CG�C�C�CC��C33CG�Cp�C�C�
C��C(�C\)Cz�C��C�C  C=qCp�C�C�RC	  C	{C	=qC	�C	��C	��C

=C
�C
\)C
�\C
�C
�HC�C33Cz�C��C�RC
=C(�CQ�C��C�C�HC(�CQ�Cp�CC�HC{CffC�CC  C�Cp�C��CC{C33CffC�C�
C
=CG�C�C�RC�
C{C\)Cz�C�C�C(�CG�Cz�C��C��C�CffC�C��C��C=qCp�C��C��C
=C=qCffC�C�C�C=qC�\CC�C�Cp�C�C�
C  CQ�C��C��C��C=qC�C�C�C33C\)C�\C�HC�CG�C�C��C  C(�Cp�CC�C�C\)C�C�C�CG�C��C��C��C Q�C �C �RC ��C!=qC!p�C!��C!�HC"33C"ffC"�\C"�
C#�C#\)C#�C#�RC$  C$Q�C$�\C$�C$��C%=qC%p�C%��C%�HC&33C&p�C&��C&��C'�C'\)C'��C'��C(  C(Q�C(�\C(�C(��C)=qC)p�C)��C)��C*(�C*\)C*�\C*��C+�C+ffC+��C+C+��C,G�C,�\C,�RC,�C-(�C-z�C-�RC-�C.{C.G�C.�\C.��C.��C/�C/ffC/��C/�C0
=C0=qC0z�C0C1  C133C1ffC1��C1�HC2(�C2G�C2z�C2C2��C3(�C3ffC3�C3�
C4  C4G�C4�\C4C4�HC533C5z�C5��C5��C6{C6\)C6�\C6�RC6�C733C7z�C7�C7�HC8{C8=qC8z�C8C8�C9�C9\)C9��C9�
C:  C:33C:ffC:��C:�
C;{C;Q�C;z�C;��C;�HC<(�C<ffC<�\C<�C<�HC=(�C=ffC=�\C=�RC=��C>=qC>p�C>��C>C?  C?=qC?z�C?��C?C@
=C@G�C@z�C@��C@��CA
=CAG�CAz�CA��CA�
CB
=CBG�CB�\CBCB�CC{CCG�CC�\CC��CC��CD{CD\)CD��CD�
CD��CE�CEffCE��CE�
CF  CF(�CFffCF�RCF�HCG{CG=qCGz�CG�RCH  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                        111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�(�A�z�A�z�A�x�A�x�A�z�A�|�A�~�AԁAԁAԁAԁA�t�A�r�A�l�A�5?A�
=A��`A��#A��
A���A���A�ȴA�ȴA�AӸRAӴ9AӰ!AӬAӧ�Aӥ�Aӟ�Aә�Aӏ\AӋDAӇ+AӃAӁA�~�A�z�A�z�A�v�A�v�A�p�A�l�A�p�A�r�A�jA�^5A�VA�S�A�M�A�K�A�K�A�E�A�=qA�/A�
=A��#A��HA��AʾwA��A��AƩ�A�C�A��A�~�A�bA�x�A���A���A��wA�t�A��jA���A�oA��A��DA�&�A�ĜA�|�A�1'A���A��
A�O�A���A�bNA���A��FA�I�A�1A��yA���A��A�33A�/A��DA���A�jA�=qA�ĜA���A��FA���A��mA��+A��!A�bNA��A���A��\A��jA�;dA���A�z�A��A�A}�A{��Ax��Aw`BAs��Ao�-Am��Ak��AiXAe��Ab�A_l�A]�A\��AY��AX�AWhsAU��AS�mAPI�AOAN��AMAJ�`AH�AG��AFE�AE�AC+AA�A<�A:�A9hsA7�PA5��A4r�A3\)A1"�A0z�A/�wA.��A.~�A-��A,�A+
=A*A�A)�TA)O�A(z�A&�A&JA$�HA#�mA#�A"=qA!�FA!
=AƨA;dA�!A��AƨAI�AdZAĜA=qA�-AXA33A�!A�A��A��A(�Ap�A+AQ�A�uA�PAS�AoA�TA33A�A\)A	
=A�AK�A
=A�jAn�A7LAG�A��A^5A�AS�A -@��@�G�@�1@���@�`B@��@���@��@�C�@��@�S�@��;@��@��;@�
=@�t�@���@�l�@��y@�p�@�1@�^@��@���@�(�@�E�@�l�@���@�-@�-@�5?@��@��@�I�@�C�@��@� �@�J@�V@ܴ9@�M�@؋D@��m@�C�@��@���@Ցh@�V@ԓu@�bN@� �@�  @��@��
@�S�@�{@Ѳ-@�X@��@Л�@�I�@ϥ�@ΰ!@·+@�v�@���@���@�Q�@�A�@�1'@���@�@���@ʗ�@�J@ɲ-@�&�@ț�@�I�@��
@�S�@��@Ƈ+@���@�hs@��@���@�9X@þw@�33@��@§�@��@�V@��9@���@�z�@�b@��@�1'@��m@�dZ@���@�~�@�M�@��@��-@��7@�hs@�/@�Ĝ@�r�@��@��!@��\@�M�@��@�{@���@�&�@��@�A�@��F@�l�@�S�@��@�
=@�ȴ@���@��h@�V@��u@��m@��@�l�@�K�@�~�@�5?@���@��@��@���@�j@��@�+@���@�M�@��^@�X@�V@��/@�(�@��@��@���@��w@��@���@��@���@���@���@���@��!@�n�@��-@��h@�7L@��`@��9@��D@��u@�Z@�Q�@�Q�@�bN@�b@��F@���@�K�@��@���@�^5@�{@�@��T@���@�O�@�&�@�V@��`@���@��@�z�@�I�@��;@�C�@��@���@�M�@�J@���@�O�@���@�bN@��@�\)@�K�@�@�v�@�M�@�E�@�5?@�{@�@��h@�`B@��@��9@��D@�1@��P@�dZ@�
=@��\@�$�@��#@��-@��@�G�@���@��u@�j@�I�@��@�t�@��H@��+@�^5@�{@���@�G�@�V@��9@�I�@���@��F@��@���@�;d@�o@�ȴ@�v�@�5?@���@��^@��7@�`B@�7L@�/@�V@���@��@��9@�A�@�b@�b@��
@�C�@�;d@�C�@�@���@��!@�^5@�-@���@���@�hs@�/@��`@���@��D@���@�r�@�b@��m@��w@���@��@�C�@���@���@���@�V@�E�@���@��@�hs@��@���@���@�bN@�A�@��@���@�\)@�;d@�o@��@��\@�M�@�5?@��@��@���@�hs@��@��@���@��D@�I�@��
@���@��@�\)@�33@�o@���@���@�^5@��+@���@�v�@�-@��@��#@��@�/@�%@��j@��u@�bN@�(�@�P@K�@~�y@~�R@~v�@~@}��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�`BA�{A�|�A�t�A�z�A�x�A�|�A�z�A�x�A�x�A�v�A�|�A�v�A�z�A�x�A�x�A�|�A�v�A�|�A�z�A�z�A�~�A�|�AԃAԁAԁAԁA�~�AԃA�~�AԅA�~�AԅAԃAԁAԇ+A�~�A�z�A�p�A�x�A�z�A�r�A�t�A�r�A�r�A�v�A�p�A�t�A�x�A�n�A�ffA�dZA�n�A�p�A�r�A�v�A�x�A�\)A�+A��A�A�
=A�A�  A�VA�VA�JA�1A��TA���A��TA��/A��HA��;A��#A��;A��/A��
A��#A��#A���A��A��A���A��A���A���A���A���A���A���A���A���A���A�ƨA���A�ƨA���A�ȴA�ƨA���A�ȴA�ƨA���A�ȴA�ƨA���A�ƨA�ĜA�ƨA���A�A�AӼjA���AӼjAӼjAӸRAӺ^AӶFAӲ-AӶFAӲ-AӲ-AӸRAӲ-AӴ9AӴ9AӮAӲ-AӲ-AӬAӲ-AӮAө�AӰ!AӬAӧ�AӬAӧ�Aӣ�Aӧ�Aӣ�Aӧ�Aө�Aӣ�Aӥ�Aӥ�Aӥ�Aӡ�Aӥ�Aӝ�Aӝ�Aӡ�Aӝ�Aӝ�Aӡ�Aӛ�Aӛ�Aӗ�AӑhAӕ�Aӏ\AӍPAӑhAӏ\AӋDAӏ\AӍPAӉ7AӍPAӉ7AӉ7AӋDAӇ+AӅAӉ7AӅAӇ+AӇ+AӃAӉ7AӃAӁAӃAӃA�~�AӅAӁAӁAӁA�~�AӁAӁA�|�A�~�A�~�A�z�A�|�A�|�A�x�A�z�A�~�A�v�A�|�A�|�A�x�A�z�A�z�A�x�A�x�A�v�A�t�A�x�A�t�A�z�A�x�A�v�A�z�A�x�A�r�A�v�A�t�A�n�A�t�A�p�A�p�A�p�A�l�A�n�A�p�A�hsA�jA�p�A�jA�p�A�t�A�l�A�r�A�p�A�l�A�t�A�r�A�t�A�t�A�n�A�r�A�jA�jA�l�A�ffA�dZA�hsA�ffA�`BA�`BA�XA�S�A�XA�XA�Q�A�VA�XA�XA�S�A�VA�XA�O�A�S�A�Q�A�M�A�Q�A�M�A�G�A�M�A�K�A�I�A�M�A�I�A�K�A�K�A�G�A�M�A�K�A�I�A�M�A�M�A�I�A�M�A�I�A�G�A�G�A�A�A�=qA�?}A�?}A�;dA�=qA�=qA�7LA�;dA�7LA�1'A�1'A�-A��A��A�bA�%A�
=A�%A�  A�A���A��A��mA�ȴAҸRAҟ�A҃A�ffA�ZA��A�ffA�;dA�AжFA�hsA��A�C�A���A�XA�/A��
A��Ḁ�A˶FA�x�A�O�A��A��#A�|�A���AɾwAɍPA�r�A�Q�A�1'A��A�JA���A���A��A��mA��TA���Aț�AȍPA�|�A�bNA�A�A�JA��mA���AǼjAǥ�Aǟ�Aǝ�Aǉ7A�l�A�Q�A�7LA�
=A��A���AƧ�A�`BA�33A��A�  A��#AŶFAš�AŋDAōPA�t�A�bNA�5?A��A�%A���A��`AĸRA�~�A�~�A�bNA�I�A�9XA� �A�A���AÇ+A�v�A�VA�7LA�bA��
A�`BA��A���A�`BA�(�A�oA���A���A��mA���A�z�A�jA�bNA�M�A�K�A�C�A�5?A�/A��A��A���A��A��DA�r�A�\)A��A��mA��jA��hA�x�A�bNA�C�A��A��A�ĜA��A���A��hA�r�A�?}A��`A��+A�^5A�;dA�bA��mA��RA��PA�M�A� �A���A��TA���A��A��hA��DA�~�A�bNA�A�A��A�1A���A��DA�=qA��A���A��9A��A�?}A�&�A��A��A��wA��wA���A���A���A�|�A�dZA�dZA�`BA�I�A� �A���A��^A��uA�`BA��A��A��wA�hsA�E�A��A�ĜA�t�A�Q�A��A�bA�%A���A���A��A��yA��TA���A��RA�x�A�&�A��A�A��mA��FA�n�A�"�A��A��A��A�{A�bA�%A��TA���A���A�ƨA��RA���A���A���A���A��PA��PA��DA�v�A�r�A�n�A�VA�-A���A�ȴA���A���A� �A���A���A��A��A��mA��;A��#A���A�ĜA��^A��hA�ZA�+A��HA��RA�`BA�I�A�C�A�C�A�A�A� �A���A�ZA��uA�33A���A���A���A�^5A�9XA��A��jA���A���A��uA��A�t�A�jA�ZA�7LA�A��A��
A�ĜA���A��hA�z�A�t�A�n�A�l�A�ffA�`BA�XA�G�A�$�A��jA�O�A�/A��A�JA�
=A��A��A�|�A�1A�bA��A�|�A�K�A�+A��A���A�VA�oA��A���A��^A���A�ffA�G�A�(�A�bA��A��wA��9A��-A��!A���A��+A�|�A�p�A�dZA�S�A�S�A�K�A�9XA� �A� �A��A�JA�JA�%A�1A�%A�
=A�A���A���A���A���A��`A��TA��HA��TA��TA��/A��#A���A�ƨA���A��^A���A�bNA�\)A�VA�S�A�E�A�33A�&�A��A�oA���A��A��A��yA��A��`A��FA�A�A�VA���A��A�ƨA��hA�v�A�S�A�=qA� �A��A�A��;A��jA��!A���A���A���A��A�|�A�n�A�p�A�ZA�C�A�5?A��A�{A�A��;A��jA��FA��-A���A���A���A��A�|�A�r�A�\)A�I�A�(�A���A�ȴA��^A��hA�t�A�=qA�"�A�A��FA��DA�p�A�=qA�&�A�
=A��`A���A���A�jA�=qA�$�A��A�{A���A��/A���A�z�A�ZA�G�A�$�A�  A��A��`A��/A�ȴA��jA��A���A��\A��A�r�A�ZA�(�A�VA��A���A�/A��A�%A��A���A�9XA�bA���A��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                        111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�(�A�z�A�z�A�x�A�x�A�z�A�|�A�~�AԁAԁAԁAԁA�t�A�r�A�l�A�5?A�
=A��`A��#A��
A���A���A�ȴA�ȴA�AӸRAӴ9AӰ!AӬAӧ�Aӥ�Aӟ�Aә�Aӏ\AӋDAӇ+AӃAӁA�~�A�z�A�z�A�v�A�v�A�p�A�l�A�p�A�r�A�jA�^5A�VA�S�A�M�A�K�A�K�A�E�A�=qA�/A�
=A��#A��HA��AʾwA��A��AƩ�A�C�A��A�~�A�bA�x�A���A���A��wA�t�A��jA���A�oA��A��DA�&�A�ĜA�|�A�1'A���A��
A�O�A���A�bNA���A��FA�I�A�1A��yA���A��A�33A�/A��DA���A�jA�=qA�ĜA���A��FA���A��mA��+A��!A�bNA��A���A��\A��jA�;dA���A�z�A��A�A}�A{��Ax��Aw`BAs��Ao�-Am��Ak��AiXAe��Ab�A_l�A]�A\��AY��AX�AWhsAU��AS�mAPI�AOAN��AMAJ�`AH�AG��AFE�AE�AC+AA�A<�A:�A9hsA7�PA5��A4r�A3\)A1"�A0z�A/�wA.��A.~�A-��A,�A+
=A*A�A)�TA)O�A(z�A&�A&JA$�HA#�mA#�A"=qA!�FA!
=AƨA;dA�!A��AƨAI�AdZAĜA=qA�-AXA33A�!A�A��A��A(�Ap�A+AQ�A�uA�PAS�AoA�TA33A�A\)A	
=A�AK�A
=A�jAn�A7LAG�A��A^5A�AS�A -@��@�G�@�1@���@�`B@��@���@��@�C�@��@�S�@��;@��@��;@�
=@�t�@���@�l�@��y@�p�@�1@�^@��@���@�(�@�E�@�l�@���@�-@�-@�5?@��@��@�I�@�C�@��@� �@�J@�V@ܴ9@�M�@؋D@��m@�C�@��@���@Ցh@�V@ԓu@�bN@� �@�  @��@��
@�S�@�{@Ѳ-@�X@��@Л�@�I�@ϥ�@ΰ!@·+@�v�@���@���@�Q�@�A�@�1'@���@�@���@ʗ�@�J@ɲ-@�&�@ț�@�I�@��
@�S�@��@Ƈ+@���@�hs@��@���@�9X@þw@�33@��@§�@��@�V@��9@���@�z�@�b@��@�1'@��m@�dZ@���@�~�@�M�@��@��-@��7@�hs@�/@�Ĝ@�r�@��@��!@��\@�M�@��@�{@���@�&�@��@�A�@��F@�l�@�S�@��@�
=@�ȴ@���@��h@�V@��u@��m@��@�l�@�K�@�~�@�5?@���@��@��@���@�j@��@�+@���@�M�@��^@�X@�V@��/@�(�@��@��@���@��w@��@���@��@���@���@���@���@��!@�n�@��-@��h@�7L@��`@��9@��D@��u@�Z@�Q�@�Q�@�bN@�b@��F@���@�K�@��@���@�^5@�{@�@��T@���@�O�@�&�@�V@��`@���@��@�z�@�I�@��;@�C�@��@���@�M�@�J@���@�O�@���@�bN@��@�\)@�K�@�@�v�@�M�@�E�@�5?@�{@�@��h@�`B@��@��9@��D@�1@��P@�dZ@�
=@��\@�$�@��#@��-@��@�G�@���@��u@�j@�I�@��@�t�@��H@��+@�^5@�{@���@�G�@�V@��9@�I�@���@��F@��@���@�;d@�o@�ȴ@�v�@�5?@���@��^@��7@�`B@�7L@�/@�V@���@��@��9@�A�@�b@�b@��
@�C�@�;d@�C�@�@���@��!@�^5@�-@���@���@�hs@�/@��`@���@��D@���@�r�@�b@��m@��w@���@��@�C�@���@���@���@�V@�E�@���@��@�hs@��@���@���@�bN@�A�@��@���@�\)@�;d@�o@��@��\@�M�@�5?@��@��@���@�hs@��@��@���@��D@�I�@��
@���@��@�\)@�33@�o@���@���@�^5@��+@���@�v�@�-@��@��#@��@�/@�%@��j@��u@�bN@�(�@�P@K�@~�y@~�R@~v�@~G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�`BA�{A�|�A�t�A�z�A�x�A�|�A�z�A�x�A�x�A�v�A�|�A�v�A�z�A�x�A�x�A�|�A�v�A�|�A�z�A�z�A�~�A�|�AԃAԁAԁAԁA�~�AԃA�~�AԅA�~�AԅAԃAԁAԇ+A�~�A�z�A�p�A�x�A�z�A�r�A�t�A�r�A�r�A�v�A�p�A�t�A�x�A�n�A�ffA�dZA�n�A�p�A�r�A�v�A�x�A�\)A�+A��A�A�
=A�A�  A�VA�VA�JA�1A��TA���A��TA��/A��HA��;A��#A��;A��/A��
A��#A��#A���A��A��A���A��A���A���A���A���A���A���A���A���A���A�ƨA���A�ƨA���A�ȴA�ƨA���A�ȴA�ƨA���A�ȴA�ƨA���A�ƨA�ĜA�ƨA���A�A�AӼjA���AӼjAӼjAӸRAӺ^AӶFAӲ-AӶFAӲ-AӲ-AӸRAӲ-AӴ9AӴ9AӮAӲ-AӲ-AӬAӲ-AӮAө�AӰ!AӬAӧ�AӬAӧ�Aӣ�Aӧ�Aӣ�Aӧ�Aө�Aӣ�Aӥ�Aӥ�Aӥ�Aӡ�Aӥ�Aӝ�Aӝ�Aӡ�Aӝ�Aӝ�Aӡ�Aӛ�Aӛ�Aӗ�AӑhAӕ�Aӏ\AӍPAӑhAӏ\AӋDAӏ\AӍPAӉ7AӍPAӉ7AӉ7AӋDAӇ+AӅAӉ7AӅAӇ+AӇ+AӃAӉ7AӃAӁAӃAӃA�~�AӅAӁAӁAӁA�~�AӁAӁA�|�A�~�A�~�A�z�A�|�A�|�A�x�A�z�A�~�A�v�A�|�A�|�A�x�A�z�A�z�A�x�A�x�A�v�A�t�A�x�A�t�A�z�A�x�A�v�A�z�A�x�A�r�A�v�A�t�A�n�A�t�A�p�A�p�A�p�A�l�A�n�A�p�A�hsA�jA�p�A�jA�p�A�t�A�l�A�r�A�p�A�l�A�t�A�r�A�t�A�t�A�n�A�r�A�jA�jA�l�A�ffA�dZA�hsA�ffA�`BA�`BA�XA�S�A�XA�XA�Q�A�VA�XA�XA�S�A�VA�XA�O�A�S�A�Q�A�M�A�Q�A�M�A�G�A�M�A�K�A�I�A�M�A�I�A�K�A�K�A�G�A�M�A�K�A�I�A�M�A�M�A�I�A�M�A�I�A�G�A�G�A�A�A�=qA�?}A�?}A�;dA�=qA�=qA�7LA�;dA�7LA�1'A�1'A�-A��A��A�bA�%A�
=A�%A�  A�A���A��A��mA�ȴAҸRAҟ�A҃A�ffA�ZA��A�ffA�;dA�AжFA�hsA��A�C�A���A�XA�/A��
A��Ḁ�A˶FA�x�A�O�A��A��#A�|�A���AɾwAɍPA�r�A�Q�A�1'A��A�JA���A���A��A��mA��TA���Aț�AȍPA�|�A�bNA�A�A�JA��mA���AǼjAǥ�Aǟ�Aǝ�Aǉ7A�l�A�Q�A�7LA�
=A��A���AƧ�A�`BA�33A��A�  A��#AŶFAš�AŋDAōPA�t�A�bNA�5?A��A�%A���A��`AĸRA�~�A�~�A�bNA�I�A�9XA� �A�A���AÇ+A�v�A�VA�7LA�bA��
A�`BA��A���A�`BA�(�A�oA���A���A��mA���A�z�A�jA�bNA�M�A�K�A�C�A�5?A�/A��A��A���A��A��DA�r�A�\)A��A��mA��jA��hA�x�A�bNA�C�A��A��A�ĜA��A���A��hA�r�A�?}A��`A��+A�^5A�;dA�bA��mA��RA��PA�M�A� �A���A��TA���A��A��hA��DA�~�A�bNA�A�A��A�1A���A��DA�=qA��A���A��9A��A�?}A�&�A��A��A��wA��wA���A���A���A�|�A�dZA�dZA�`BA�I�A� �A���A��^A��uA�`BA��A��A��wA�hsA�E�A��A�ĜA�t�A�Q�A��A�bA�%A���A���A��A��yA��TA���A��RA�x�A�&�A��A�A��mA��FA�n�A�"�A��A��A��A�{A�bA�%A��TA���A���A�ƨA��RA���A���A���A���A��PA��PA��DA�v�A�r�A�n�A�VA�-A���A�ȴA���A���A� �A���A���A��A��A��mA��;A��#A���A�ĜA��^A��hA�ZA�+A��HA��RA�`BA�I�A�C�A�C�A�A�A� �A���A�ZA��uA�33A���A���A���A�^5A�9XA��A��jA���A���A��uA��A�t�A�jA�ZA�7LA�A��A��
A�ĜA���A��hA�z�A�t�A�n�A�l�A�ffA�`BA�XA�G�A�$�A��jA�O�A�/A��A�JA�
=A��A��A�|�A�1A�bA��A�|�A�K�A�+A��A���A�VA�oA��A���A��^A���A�ffA�G�A�(�A�bA��A��wA��9A��-A��!A���A��+A�|�A�p�A�dZA�S�A�S�A�K�A�9XA� �A� �A��A�JA�JA�%A�1A�%A�
=A�A���A���A���A���A��`A��TA��HA��TA��TA��/A��#A���A�ƨA���A��^A���A�bNA�\)A�VA�S�A�E�A�33A�&�A��A�oA���A��A��A��yA��A��`A��FA�A�A�VA���A��A�ƨA��hA�v�A�S�A�=qA� �A��A�A��;A��jA��!A���A���A���A��A�|�A�n�A�p�A�ZA�C�A�5?A��A�{A�A��;A��jA��FA��-A���A���A���A��A�|�A�r�A�\)A�I�A�(�A���A�ȴA��^A��hA�t�A�=qA�"�A�A��FA��DA�p�A�=qA�&�A�
=A��`A���A���A�jA�=qA�$�A��A�{A���A��/A���A�z�A�ZA�G�A�$�A�  A��A��`A��/A�ȴA��jA��A���A��\A��A�r�A�ZA�(�A�VA��A���A�/A��A�%A��A���A�9XA�bA���A��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                        111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�B�B�TB�B��B��B�B�B�TB�B�B�B�B�B�GB�B��B�B�B��B�iB�B��B�iB��B��B�B�cB�B��B�/B��B�)B��B�QB�B�KB�B�B�yB��B�B�B�"B�"B��B�B�B�]B�B�WB�B�B��B�]B�WB�B��B��B�B@B�B��B�\B�\B��B��B��B��B~�B{ByrBwfBo5Bj�B_�B_pBP}BK�BJ#B;�B3hB+6B�B�BxBAB��B�pB�gB��B�jB�)BɆB�-B�}B�_B�'B�	B��B�xB.BsBe�B[WBGB8�B �BJBoB
�B
ɆB
�[B
��B
�\B
�GB
tB
aHB
D�B
49B
VB
bB
fB	�"B	��B	��B	��B	�!B	�IB	�MB	|B	s�B	l�B	`vB	XB	QNB	GEB	9�B	.�B	+kB	)�B	#nB	�B	�B	B	�B�"B��B�/B��B�B�pB�/B��B�EB�/B�sB�QBیB��B�;B��B��B��B��B�B�KB��B�B�;B�	B��B	�B	�B	B	\B	�B	�B	
	B	�B	�B	�B	1B	1B	_B	�B	
�B	.B	B	VB	B	DB	�B	�B	%B	B	 �B��B�B�+B��B�`B��B�DB�`B�ZB�B�B�BB�
BߤB�HB� B�TB�&B�B�HB�pB�B��B�/B��B�8B	{B	�B		�B	�B	 �B	)_B	/�B	7B	<�B	?�B	A�B	@B	?B	A�B	>B	=<B	9XB	2�B	2�B	5�B	1�B	4�B	=<B	LdB	P�B	P�B	PHB	OBB	M�B	GB	C�B	>BB	C-B	C-B	@�B	A�B	CaB	D�B	D�B	F�B	F�B	H�B	L�B	N�B	OB	N�B	N�B	T�B	Z�B	[#B	Z�B	Z�B	[WB	[�B	bNB	b�B	c B	b�B	d�B	i�B	j�B	j�B	j�B	k�B	p�B	qB	q�B	sMB	s�B	u%B	w�B	y	B	{B	|�B	}�B	�oB	�{B	�YB	�+B	�B	��B	��B	�\B	� B	��B	�B	��B	�@B	�FB	��B	��B	�eB	�B	��B	�CB	�~B	�B	��B	��B	��B	��B	�\B	��B	��B	�bB	�B	��B	�FB	��B	�$B	��B	��B	�*B	�XB	��B	��B	�qB	�B	�CB	�B	�B	��B	��B	��B	��B	�nB	�zB	��B	��B	��B	��B	��B	��B	�B	B	B	�tB	�EB	�B	ɺB	��B	��B	�jB	͟B	бB	ҽB	��B	�B	ںB	��B	ܒB	�B	�B	�B	�B	�|B	�B	� B	��B	��B	�B	��B	��B	�fB	�B	�B	��B	�B	��B	�B	�]B	� B	� B	� B	��B	�B	�MB	�B	�TB	��B	��B	��B	��B	�+B	��B	��B	��B	�2B	��B	�xB	�B	��B	�PB	�PB	�"B	�"B	��B	�]B	��B	�]B	�]B	��B	�.B	��B	��B	��B
 4B
 iB
 iB
 �B
uB
MB
�B
�B
�B
B
%B
�B
fB
	�B
	�B

=B

�B
�B
B
xB
xB
�B
B
~B
B
~B
�B
�B
�B
�B
�B
\B
�B
�B
�B
�B
�B
bB
hB
�B
B
�B
�B
B
�B
�B
B
kB
kB
7B
�B
7B
kB
7B
	B
�B
B
B
�B
�B
VB
VB
�B
 \B
 'B
�B
�B
 �B
 'B
 �B
!bB
!�B
!bB
!�B
!�B
!�B
"hB
#B
"�B
!�B
!-B
 �B
!�B
"hB
"hB
!�B
"hB
#�B
#nB
#nB
#�B
&B
'�B
&�B
&�B
&�B
'RB
(XB
(�B
)_B
)�B
+B
+�B
+�B
,qB
,�B
,�B
,=B
+�B
+�B
-B
-CB
-CB
-�B
-�B
.IB
.B
-�B
/OB
0�B
1�B
0�B
/�B
1[B
2�B
3hB
3�B
3�B
49B
4�B
5B
4�B
5B
4�B
4nB
4nB
4nB
4G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�v�?�}V@H��B�B�+B�B�ZB��B�B��B�TB�ZB�B��B�B��B�ZB�B�+B�B��B�B�B��B�MB�TB�B�B��B�B��B�B��B�B�B��B��B�+B�B�B�AB�MB��B��B�B�B�GB�B�B�GB�B��B�B�B��B�GB�MB��B�JB�BB�GB��B�B��B�]B��B�cB�B�TB�B� B�B��B�iB�B�B�5B��B��B� B�B�iB� B�B�cB�oB�B�B�;B�B�cB�B�B�/B�B�)B�5B�/B�/B�iB��B�iB��B�cB�B��B�/B�B�B�/B�B�cB�cB�B��B��B��B�B��B�5B�B�B��B��B�]B�iB�iB�/B�B�/B��B��B�B�B�5B��B��B��B��B��B� B�B�cB�]B�]B�iB�B�)B��B� B�]B��B�/B�WB��B�/B��B�5B��B��B��B�B�WB��B��B�QB��B�B�B�"B�B�B�"B�B�B��B�B�B�KB�B�B�DB�QB��B��B�yB�B��B�B�B�sB��B�B�yB�B�DB�B�KB�B�sB�yB��B�B�QB�yB��B��B��B�B�B�DB�yB�KB�>B��B��B�QB�WB�B�WB��B�B�B�WB�B�B�B��B��B��B�B�B��B�B��B�B�B�;B�WB��B��B�"B�5B��B��B�oB�/B�;B�B�]B��B��B�)B�B�B��B�)B��B�B�B�]B�B�B�B�]B�B�B�B�B�WB�B�B��B��B�B�B�]B�B�]B�B�"B��B�B��B�B�B�WB��B�B�B��B�WB��B�cB�"B�B�]B��B��B��B�B�B�QB�DB�mB�B�2B��B�mB�&B��B�B�B��B�fB�
B�B�B�mB�B��B�BB�BYB~B�B	B/�B=qBD�BI�BIBYKBuZB��B��B��B��B��B�@B�B�B��B��B��B�B��B��B��B�:B�(B�(B�(B��B�oB�oB�\B��B��B��B��B��B�bB��B�VB�B�B��B��B��B��B��B��B��B�"B��B�bB��B�B��B��B�B�B��B��B��B�bB�rB��B�B��B�.B�+B�fB�1B��B�+B�_B��B�B��B�uB��B��B�B��B�PB��B��B�_B�+B��B�{B��B�AB�B��B�oB� B�B.B}�B� B~�B��B�B�B�iB�4B~�B.B��B�uB��B~]B�B}�B|B�B�iB{JBzBx�By	Bx�B�4B.B��By>Bx�B{Bz�By�B}�B~(B{B}VBw�Bz�Bz�ByrBu�BwfBz�Bx8Bx�Bu�B{B|�B~�Bv+Bv`Bv�By�BtBqABpoBv�BsBo�Bl�Bt�Bp;Br�Br�Bl"Bl"BoiBp;BncBm�Bm�BncBkBg�Bj�Bo�Bd�Bf2Bo�Bc�Bd&BgB\�B^jB`BB]�B^5B]dB\�B^jB^�Bh�Bb�Bx�B]dBX�B\�B`BXBR BP}BQ�BQ�BOBR�BR�BOBBN�BO�BN�BPBN<BL�BK)BMBJ�BJ#BK�BIRBG�BL�BI�BN�BB�BF�Bh
BK^BAUB=qB=�B;�B<6B;dB9�B9�B9$B9�BC-B8�B:�B>BB7LB9�B1�B-CB+6B(�B/�B33B9�B;dB*�B$�B"hB#nB$tB �B!�B�BeB�B�B$B�B�B{B7BB�B�BbBB.B�BBB
	B
=B
�B	7B�B
rB1B
�B�B�B�]B�B�"B��BSB+B�B��B�B�"B�KB�)B�QB� B�&B��B�HB�pB�;B�B�B��B�B��BרB��B�[BҽBӏB�aB� B�NB�B҉BϫBϫBԕBΥB�}BҽB�jB̘B��B�B�6B�dB�6B͟B�^B��B��BʌB��B˒B��BɺB�RB�^BʌB�^B�zB�RB�0B�XB�mB�B�mB�KB�3B�B� BB�9B�B��B��B��B�B�-B�B��BǮB��B�B��B�B��B��B��B�@B�$B�B�*B�B�\B��B�\B�hB��B�VB��B��B��B�~B��B�eB�qB��B�eB��B�B��B��B�{B��B�uB�B� B��B��B�SB�MB�B��B��B��B�GB�B��B��B��B��B�B� B�B|�B�GB}VB|�Bw�Bu�BtTBv�Bu�Bx8Bv`BpBncBqBk�BjBi�BgBh
Be�Be�B`vB`Be`B^�Bf2BaB\�B\�Bh
B[�BR�BO�BO�B\�BUgBHKBG�BF944444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                        444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�944444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                        444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022122608024120221226080241IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023010503013720230105030137QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023010503013720230105030137QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             SI  SI  ARFMARFM                                                                                                                                                2023021013194820230210131948IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                